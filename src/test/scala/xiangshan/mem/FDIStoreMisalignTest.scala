// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.control.NonFatal
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput}
import xiangshan.backend.fu.FuType
import xiangshan.cache.MemoryOpConstants

class FDIStoreMisalignTest extends AnyFlatSpec {
  behavior of "Scalar misaligned stores through production splitting and publication"

  private val enabled = sys.env.getOrElse("L05_FDI_ENABLED",
    throw new IllegalArgumentException("L05_FDI_ENABLED must be explicit")).toBoolean
  private val scenario = sys.env.getOrElse("L05_SCENARIO", "baseline")
  private val wordMask = (BigInt(1) << 64) - 1

  private def bool(data: Bool): Boolean = data.peek().litToBoolean
  private def uint(data: UInt): BigInt = data.peek().litValue
  private def zero(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(zero)
    case value: Record => value.elements.values.foreach(zero)
    case other => throw new IllegalArgumentException(s"Unsupported input ${other.getClass.getName}")
  }
  private def zeroInputs(data: Data): Unit = data match {
    case value: Vec[_] => value.foreach(zeroInputs)
    case value: Record => value.elements.values.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => zero(leaf)
    case _ =>
  }
  private def faults(bits: Vec[Bool]): Set[Int] =
    bits.zipWithIndex.collect { case (bit, index) if bool(bit) => index }.toSet

  private def observedExceptionAddress(dut: FDIStoreMisalignHarness): BigInt = {
    val raw = uint(dut.view.exceptionAddress)
    if (!bool(dut.view.exceptionNeedsExtension)) raw
    else {
      // Reproduce the real MemBlock consumer's representation conversion, not the expected VA.
      val mode = uint(dut.io.tlbCsr.priv.dmode).toInt
      val virtual = bool(dut.io.tlbCsr.priv.virt) || bool(dut.view.exceptionIsHyper)
      val satp = uint(dut.io.tlbCsr.satp.mode).toInt
      val vsatp = uint(dut.io.tlbCsr.vsatp.mode).toInt
      val hgatp = uint(dut.io.tlbCsr.hgatp.mode).toInt
      val formats = Seq(
        (virtual && vsatp == 0 && hgatp == 0 || !virtual && (mode == 3 || satp == 0), dut.PAddrBits, false),
        (virtual && vsatp == 8 || !virtual && mode != 3 && satp == 8, 39, true),
        (virtual && vsatp == 9 || !virtual && mode != 3 && satp == 9, 48, true),
        (virtual && vsatp == 0 && hgatp == 8, 41, false),
        (virtual && vsatp == 0 && hgatp == 9, 50, false)).filter(_._1)
      assert(formats.size == 1, "Exception address did not select one native extension format")
      val (_, bits, signed) = formats.head
      val mask = (BigInt(1) << bits) - 1
      val low = raw & mask
      if (signed && low.testBit(bits - 1)) low | (wordMask ^ mask) else low
    }
  }

  private case class Trial(name: String, address: BigInt = 0x800d, operation: Int = 3,
    floating: Boolean = false, translated: Boolean = false, denied: Boolean = false,
    highPageFault: Boolean = false, highAccessFault: Boolean = false, highNoncache: Boolean = false,
    pmm: Int = 0, translationMode: Int = 8, ptwDelay: Int = 10,
    highProtection: String = "none", evictLow: Boolean = false, dataDelay: Int = 0) {
    require(Set("none", "pmp-write", "pma-write", "pma-noncache")(highProtection))
    require(highProtection == "none" || translated && !highPageFault && !highAccessFault &&
      (!highNoncache || highProtection == "pmp-write"))
    require(!evictLow || translated)
    val size: Int = 1 << operation
    val data: BigInt = BigInt("89abcdef76543210", 16)
    val effective: BigInt = {
      val keep = if (pmm == 3) 48 else if (pmm == 2) 57 else 64
      val mask = (BigInt(1) << keep) - 1
      val low = address & mask
      if (translated && keep < 64 && low.testBit(keep - 1)) low | (wordMask ^ mask) else low
    }
    val lowPage: BigInt = effective >> 12
    val highAddress: BigInt = ((effective & ~BigInt(15)) + 16) & wordMask
    val cross16: Boolean = (effective >> 4) != ((effective + size - 1) >> 4)
    val crossPage: Boolean = (effective >> 12) != ((effective + size - 1) >> 12)
    val lowBound: BigInt = effective & ~BigInt(4095)
    val highBound: BigInt = if (denied) highAddress else lowBound + 8192
    val fdiDenied: Boolean = enabled && effective + size - 1 >= highBound
    val canonicalBits: Int = if (translationMode == 9) 48 else 39
    val highCanonicalFault: Boolean = translated && crossPage &&
      (highAddress >> canonicalBits) !=
        (if (highAddress.testBit(canonicalBits - 1)) (BigInt(1) << (64 - canonicalBits)) - 1 else BigInt(0))
    val standardCause: Option[Int] = if (highPageFault || highCanonicalFault) Some(15)
      else if (highAccessFault || highProtection == "pmp-write" || highProtection == "pma-write") Some(7)
      else if (highNoncache || highProtection == "pma-noncache") Some(6) else None
    val expectedCause: Option[Int] = standardCause.orElse(if (fdiDenied) Some(24) else None)
    val expectedFaultAddress: BigInt = if (standardCause.exists(cause => cause == 15 || cause == 7)) highAddress else effective
    def physical(address: BigInt): BigInt = if (!translated) address else {
      val high = (address >> 12) != lowPage
      val ppn = if (high && highAccessFault) BigInt(0x200) else if (high) BigInt(0xa0) else BigInt(0x80)
      (ppn << 12) | (address & 4095)
    }
    val expectedBytes: Map[BigInt, Int] = if (expectedCause.nonEmpty) Map.empty else
      (0 until size).map(offset => physical(effective + offset) -> ((data >> (8 * offset)) & 255).toInt).toMap
  }
  private val baselineCases = Seq(
    Trial("allowed-cross16"),
    Trial("allowed-cross-page", address = 0x8ffd, translated = true),
    Trial("denied-high-page-pf", address = 0x8ffd, translated = true, denied = true, highPageFault = true),
    Trial("high-page-pf", address = 0x8ffd, translated = true, highPageFault = true),
    Trial("denied-cross-page", address = 0x8ffd, translated = true, denied = true))
  private val negativePage = BigInt("ffffffc000008000", 16)
  private val pmm16Limit = BigInt(1) << 47
  private val within16Cases = Seq(
    Trial("integer-half-within16", address = 0x8001, operation = 1),
    Trial("floating-word-within16", address = 0x8001, operation = 2, floating = true),
    Trial("floating-double-within16", address = 0x8007, floating = true))
  private val dataCases = within16Cases ++ Seq(
    Trial("integer-half-cross16", address = 0x800f, operation = 1),
    Trial("integer-word-cross16", address = 0x800e, operation = 2),
    Trial("integer-double-cross16-offset9", address = 0x8009),
    Trial("integer-double-cross16-offset15", address = 0x800f),
    Trial("floating-word-cross16", address = 0x800d, operation = 2, floating = true),
    Trial("floating-double-cross16", address = 0x800b, floating = true),
    Trial("integer-half-cross-page", address = 0x8fff, operation = 1, translated = true),
    Trial("integer-word-cross-page", address = 0x8ffe, operation = 2, translated = true),
    Trial("floating-word-cross-page", address = 0x8ffd, operation = 2, floating = true, translated = true),
    Trial("floating-double-cross-page", address = 0x8ffb, floating = true, translated = true),
    Trial("integer-delayed-std", address = 0x8ffd, translated = true, dataDelay = 40),
    Trial("floating-delayed-std", address = 0x800d, floating = true, dataDelay = 20))
  private val standardCases = Seq(
    Trial("high-page-af", address = 0x8ffd, translated = true, highAccessFault = true),
    Trial("denied-high-page-af", address = 0x8ffd, translated = true, denied = true, highAccessFault = true),
    Trial("high-page-nc", address = 0x8ffd, translated = true, highNoncache = true),
    Trial("denied-high-page-nc", address = 0x8ffd, translated = true, denied = true, highNoncache = true),
    Trial("floating-high-page-pf", address = 0x8ffd, floating = true, translated = true, highPageFault = true),
    Trial("high-page-pmp-write-denied", address = 0x8ffd, translated = true, highProtection = "pmp-write"),
    Trial("denied-high-page-pmp-write", address = 0x8ffd, translated = true, denied = true, highProtection = "pmp-write"),
    Trial("high-page-pma-write-denied", address = 0x8ffd, translated = true, highProtection = "pma-write"),
    Trial("denied-high-page-pma-write", address = 0x8ffd, translated = true, denied = true, highProtection = "pma-write"),
    Trial("high-page-nc-pmp-write-denied", address = 0x8ffd, translated = true,
      highNoncache = true, highProtection = "pmp-write"),
    Trial("denied-high-page-nc-pmp-write", address = 0x8ffd, translated = true, denied = true,
      highNoncache = true, highProtection = "pmp-write"),
    Trial("high-page-pma-noncache", address = 0x8ffd, translated = true, highProtection = "pma-noncache"),
    Trial("denied-high-page-pma-noncache", address = 0x8ffd, translated = true, denied = true, highProtection = "pma-noncache"))
  private val addressCases = Seq(
    Trial("sv39-high-allowed", address = negativePage + 0xffd, translated = true),
    Trial("sv39-high-page-pf", address = negativePage + 0xffd, translated = true, highPageFault = true),
    Trial("sv39-high-denied-page-pf", address = negativePage + 0xffd, translated = true, denied = true, highPageFault = true),
    Trial("bare-pmm16-cross16", address = (BigInt(0xabcd) << 48) | 0x800d, pmm = 3),
    Trial("sv48-pmm16-boundary-allowed", address = (BigInt(0xabcd) << 48) | (pmm16Limit - 3),
      translated = true, translationMode = 9, pmm = 3),
    Trial("sv48-pmm16-boundary-denied", address = (BigInt(0xabcd) << 48) | (pmm16Limit - 3),
      translated = true, translationMode = 9, pmm = 3, denied = true))
  private val retryCases = Seq(
    Trial("long-translation-retry", address = 0x8ffd, translated = true, ptwDelay = 40),
    Trial("denied-long-high-translation", address = 0x8ffd, translated = true, denied = true, ptwDelay = 40),
    Trial("low-fragment-translation-retry", address = 0x8ffd, translated = true, evictLow = true))
  private val groups = Map("baseline" -> baselineCases.take(4), "data" -> dataCases,
    "standard" -> standardCases, "addresses" -> addressCases, "retries" -> retryCases,
    "within16" -> within16Cases, "supplemental" -> within16Cases)
  private val cases = baselineCases ++ dataCases ++ standardCases ++ addressCases ++ retryCases
  private val pressureScenario = "real-dual-publication-backpressure"
  private val cancellationScenarios = Seq("cancel-sta-s1-new-owner", "cancel-smb-candidate-s2-new-owner",
    "cancel-high-fragment-s2-new-owner")
  private val deferredCancellationScenarios = if (enabled)
    Seq("cancel-probe-s2-new-owner", "cancel-terminal-s1-new-owner", "cancel-terminal-s2-new-owner") else Seq.empty[String]
  require(scenario == "acceptance" || scenario == pressureScenario || scenario == "cancellation" || cancellationScenarios.contains(scenario) ||
    enabled && scenario == "deferred-cancellation" || deferredCancellationScenarios.contains(scenario) ||
    scenario == "all" || groups.contains(scenario) || cases.exists(_.name == scenario),
    s"Unsupported L05_SCENARIO: $scenario")
  private case class SqIdentity(flag: Boolean, value: Int)
  private case class PtwReply(due: Int, vpn: BigInt, sq: Int)

  private class Driver(dut: FDIStoreMisalignHarness, trial: Trial, events: mutable.ArrayBuffer[String]) {
    private val rob = 10
    private val lane = 0
    private var cycle = 0
    private var sq = SqIdentity(false, 0)
    private var allocated = false
    private var attempts = 0
    private var firstIssueCycle = -1
    private var fenceAt = -1
    private var releaseHeadAt = -1
    private var lowRetries = 0
    private var highRetries = 0
    private var retryAt = -1
    private var waitingAddressFeedback = false
    private var dataAccepted = false
    private var dataResults = 0
    private var completions = 0
    private var lastCompletion = -1
    private var headReleased = false
    private var publications = 0
    private var emptyTokens = 0
    private var byteWrites = 0
    private var dequeues = 0
    private var splitRequests = 0
    private var splitRetries = 0
    private var highTranslations = 0
    private var metadataRequests = 0
    private var cacheWrites = 0
    private var uncacheWrites = 0
    private val publishedBytes = mutable.Map.empty[BigInt, Int]
    private val memoryBytes = mutable.Map.empty[BigInt, Int]
    private val metadataResponses = Seq.fill(dut.io.metadata.length)(mutable.Queue.empty[Int])
    private val acknowledgements = mutable.Queue.empty[(Int, BigInt)]
    private val ptwReplies = mutable.ArrayBuffer.empty[PtwReply]
    private val scheduledVpns = mutable.Set.empty[BigInt]
    private val vaddrMask = (BigInt(1) << dut.VAddrBits) - 1
    private val lowVpn = (trial.address & vaddrMask) >> 12
    private val highVpn = lowVpn + 1

    private def event(kind: String, fields: String = ""): Unit = events +=
      s"{\"case\":\"${trial.name}\",\"cycle\":$cycle,\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    private def owner(uop: DynInst): Unit = {
      uop.robIdx.flag.expect(false.B); uop.robIdx.value.expect(rob.U)
      uop.sqIdx.flag.expect(sq.flag.B); uop.sqIdx.value.expect(sq.value.U)
      uop.uopIdx.expect(0.U)
    }
    private def driveOwner(uop: DynInst): Unit = {
      zero(uop)
      val instruction = (BigInt(1) << 20) | (BigInt(2) << 15) |
        (BigInt(trial.operation) << 12) | (if (trial.floating) 0x27 else 0x23)
      uop.instr.poke(instruction.U); uop.pc.poke(0x4000.U)
      uop.fuType.poke(FuType.stu.U); uop.fuOpType.poke(trial.operation.U)
      uop.robIdx.value.poke(rob.U)
      uop.sqIdx.flag.poke(sq.flag.B); uop.sqIdx.value.poke(sq.value.U)
      uop.firstUop.poke(true.B); uop.lastUop.poke(true.B); uop.numLsElem.poke(1.U)
      uop.fdiNotTrusted.foreach(_.poke(true.B))
    }
    private def driveAddress(): Unit = {
      val port = dut.io.sta(lane)
      zero(port.bits); driveOwner(port.bits.uop)
      port.bits.src(0).poke(trial.address.U); port.bits.src(1).poke(trial.data.U)
      port.bits.isFirstIssue.poke((attempts == 0).B); port.valid.poke(true.B)
    }
    private def atHead(): Unit = {
      // Cross-page SMB translation requires ROB head ownership. In this native SQ,
      // pendingPtr also authorizes scalar commit; scommit=0 alone is not a write gate.
      dut.io.rob.pendingPtr.value.poke(rob.U)
      dut.io.rob.pendingPtrNext.value.poke(rob.U)
      dut.io.rob.pendingst.poke(true.B)
      headReleased = true
    }
    private def receiveCompletion(wb: MemExuOutput, source: String): Unit = {
      owner(wb.uop)
      val actual = faults(wb.uop.exceptionVec)
      event("store-writeback", s"\"path\":\"$source\",\"rob\":$rob,\"sq\":${sq.value},\"exceptions\":[${actual.toSeq.sorted.mkString(",")}]")
      val expected = trial.expectedCause.toSet
      val permitted = expected ++ (if (trial.fdiDenied) Set(24) else Set.empty[Int])
      // Local vectors may retain both candidates; Backend retirement must select the standard fault.
      assert(expected.subsetOf(actual) && (actual -- permitted).isEmpty,
        s"${trial.name}: expected store exception $expected but saw $actual")
      assert(completions == 0, "Store owner completed more than once")
      completions += 1; lastCompletion = cycle
      if (!headReleased) atHead()
    }
    private def step(): Unit = {
      dut.io.sfence.valid.poke((cycle == fenceAt).B)
      if (cycle == fenceAt) {
        dut.io.sfence.bits.rs1.poke(true.B); dut.io.sfence.bits.rs2.poke(true.B)
        scheduledVpns.clear()
        event("evict-translated-low-page")
      }
      if (cycle == releaseHeadAt) atHead()
      for ((port, index) <- dut.io.metadata.zipWithIndex) {
        port.resp.valid.poke(metadataResponses(index).headOption.contains(cycle).B)
        if (metadataResponses(index).headOption.contains(cycle)) metadataResponses(index).dequeue()
      }
      dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (acknowledgements.headOption.exists(_._1 == cycle)) {
        val (_, id) = acknowledgements.dequeue()
        dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        dut.io.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
      }
      dut.io.ptw.resp.valid.poke(false.B)
      ptwReplies.find(_.due == cycle).foreach { reply =>
        val response = dut.io.ptw.resp.bits
        zero(response)
        val high = reply.vpn == highVpn
        val pf = high && trial.highPageFault
        val ppn = if (high && trial.highAccessFault) BigInt(0x200) else if (high) BigInt(0xa0) else BigInt(0x80)
        val sector = response.s1.pteidx.length
        val index = (reply.vpn % sector).toInt
        response.memidx.is_st.poke(true.B); response.memidx.idx.poke(reply.sq.U)
        response.s1.entry.tag.poke((reply.vpn / sector).U)
        response.s1.entry.level.foreach(_.poke(0.U))
        response.s1.entry.ppn.poke((ppn / sector).U)
        response.s1.entry.v.poke((!pf).B)
        response.s1.entry.pbmt.poke((if (high && trial.highNoncache) 1 else 0).U)
        response.s1.entry.perm.foreach { perm =>
          perm.r.poke(true.B); perm.w.poke(true.B); perm.a.poke(true.B); perm.d.poke(true.B); perm.u.poke(true.B)
        }
        response.s1.addr_low.poke(index.U); response.s1.pf.poke(pf.B)
        response.s1.valididx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.pteidx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.ppn_low.zipWithIndex.foreach { case (part, i) => part.poke((if (i == index) ppn % sector else BigInt(i)).U) }
        dut.io.ptw.resp.valid.poke(true.B); dut.io.ptw.resp.ready.expect(true.B)
        event("ptw-response", s"\"vpn\":${reply.vpn},\"ppn\":$ppn,\"pf\":$pf")
      }
      for (request <- dut.io.ptw.req if bool(request.valid) && bool(request.ready)) {
        val vpn = uint(request.bits.vpn)
        assert(trial.translated && Set(lowVpn, highVpn)(vpn), "Store PTW escaped its original pages")
        request.bits.memidx.is_st.expect(true.B); request.bits.memidx.idx.expect(sq.value.U)
        if (vpn == highVpn) highTranslations += 1
        if (scheduledVpns.add(vpn)) ptwReplies += PtwReply(cycle + trial.ptwDelay, vpn, sq.value)
        event("ptw-request", s"\"vpn\":$vpn")
      }
      for ((port, index) <- dut.io.metadata.zipWithIndex if bool(port.req.valid) && bool(port.req.ready)) {
        // StoreUnit requests metadata/prefetch service here; the data write is downstream of SQ.
        port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
        metadataResponses(index).enqueue(cycle + 2)
        metadataRequests += 1
        event("metadata-query", s"\"lane\":$index,\"vaddr\":${uint(port.req.bits.vaddr)}")
      }
      if (bool(dut.io.sta(lane).valid) && bool(dut.io.sta(lane).ready)) {
        owner(dut.io.sta(lane).bits.uop)
        attempts += 1; waitingAddressFeedback = true
        if (firstIssueCycle < 0) firstIssueCycle = cycle
        event("original-sta", s"\"attempt\":$attempts,\"fullva\":${trial.effective}")
      }
      if (bool(dut.io.std(lane).valid) && bool(dut.io.std(lane).ready)) {
        assert(!dataAccepted, "STD source was accepted twice")
        dataAccepted = true
        event("std-accepted")
      }
      for (feedback <- dut.io.feedback if bool(feedback.valid)) {
        if (!bool(feedback.bits.hit)) retryAt = cycle + trial.ptwDelay + 6
        waitingAddressFeedback = false
        event("address-feedback", s"\"hit\":${bool(feedback.bits.hit)}")
      }
      if (trial.evictLow && fenceAt < 0 && bool(dut.view.splitEnqueue(lane).valid) &&
        bool(dut.io.phases(lane).tlbResponse.valid) && !bool(dut.io.phases(lane).tlbResponse.bits.miss)) {
        owner(dut.view.splitEnqueue(lane).bits.uop)
        assert(!headReleased && !bool(dut.io.slots(sq.value).committed),
          "Low-page eviction requires an uncommitted cross-page owner before ROB head")
        fenceAt = cycle + 1
        releaseHeadAt = fenceAt + dut.sttlbParams.fenceDelay + 4
      }
      if (bool(dut.io.splitRequestFire)) {
        owner(dut.io.splitRequest.bits.uop)
        splitRequests += 1
        event("split-request", s"\"vaddr\":${uint(dut.io.splitRequest.bits.vaddr)},\"fullva\":${uint(dut.io.splitRequest.bits.fullva)},\"high\":${bool(dut.io.splitRequest.bits.isFinalSplit)}")
      }
      if (bool(dut.view.splitResponse.valid)) {
        owner(dut.view.splitResponse.bits.uop)
        if (bool(dut.view.splitResponse.bits.need_rep)) {
          splitRetries += 1
          if (bool(dut.view.splitResponse.bits.isFinalSplit)) highRetries += 1 else lowRetries += 1
        }
        event("split-response", s"\"retry\":${bool(dut.view.splitResponse.bits.need_rep)},\"exceptions\":[${faults(dut.view.splitResponse.bits.uop.exceptionVec).toSeq.sorted.mkString(",")}]")
      }
      for (wb <- dut.io.wb if bool(wb.valid)) receiveCompletion(wb.bits, "sta")
      if (bool(dut.view.splitWriteback.valid)) receiveCompletion(dut.view.splitWriteback.bits, "smb")
      for (wb <- dut.io.dataWb if bool(wb.valid)) {
        owner(wb.bits.uop); dataResults += 1
        assert(dataResults == 1, "STD completed more than once")
      }
      for ((port, index) <- dut.io.publication.zipWithIndex) {
        val write = dut.io.writes(index)
        assert(bool(write.valid) == (bool(port.fire) && bool(port.bits.vecValid)),
          "Sbuffer byte writes must match nonempty publication acceptance")
        if (bool(port.fire)) {
          port.sq.flag.expect(sq.flag.B); port.sq.value.expect(sq.value.U)
          publications += 1
          if (!bool(port.bits.vecValid)) emptyTokens += 1
          else {
            assert(trial.expectedCause.isEmpty, "Faulting store published target bytes")
            val base = uint(port.bits.addr) & ~BigInt(15)
            val mask = uint(port.bits.mask)
            if (!trial.cross16) {
              val expectedMask = ((BigInt(1) << trial.size) - 1) << (trial.effective & 15).toInt
              assert(mask == expectedMask, "Within-16B store publication has an incorrect byte mask")
            }
            for (offset <- 0 until port.bits.mask.getWidth if mask.testBit(offset)) {
              val address = base + offset
              val data = ((uint(port.bits.data) >> (offset * 8)) & 255).toInt
              assert(trial.expectedBytes.get(address).contains(data), s"Unexpected published store byte at $address")
              assert(!publishedBytes.contains(address), "Store byte was published more than once")
              publishedBytes(address) = data
            }
          }
          event("sq-publication", s"\"lane\":$index,\"mask\":${uint(port.bits.mask)},\"vec_valid\":${bool(port.bits.vecValid)},\"sq_need_deq\":${bool(port.bits.sqNeedDeq)}")
        }
        if (bool(write.valid)) byteWrites += 1
      }
      dequeues += uint(dut.io.queueDequeue).toInt
      if (bool(dut.io.uncache.req.valid)) {
        uncacheWrites += 1
        event("uncache-write", s"\"address\":${uint(dut.io.uncache.req.bits.addr)}")
        assert(false, "Misaligned store must not publish an uncached target write")
      }
      val cache = dut.io.cacheWrite.req
      if (bool(cache.valid) && bool(cache.ready)) {
        cache.bits.cmd.expect(MemoryOpConstants.M_XWR)
        cacheWrites += 1
        val base = uint(cache.bits.addr)
        val mask = uint(cache.bits.mask)
        assert(trial.expectedCause.isEmpty, "Faulting store sent a target cache write")
        for (offset <- 0 until cache.bits.mask.getWidth if mask.testBit(offset)) {
          val address = base + offset
          val data = ((uint(cache.bits.data) >> (offset * 8)) & 255).toInt
          assert(trial.expectedBytes.get(address).contains(data), s"Unexpected cache store byte at $address")
          assert(!memoryBytes.contains(address), "Cache accepted a duplicate store byte")
          memoryBytes(address) = data
        }
        acknowledgements.enqueue((cycle + 3, uint(cache.bits.id)))
        event("cache-write", s"\"address\":$base,\"mask\":$mask")
      }
      dut.clock.step(); cycle += 1
    }

    def run(): Unit = {
      zeroInputs(dut.io)
      dut.io.enq.lqCanAccept.poke(true.B)
      dut.io.metadata.foreach(_.req.ready.poke(true.B))
      dut.io.ptw.req.foreach(_.ready.poke(true.B))
      dut.io.uncache.req.ready.poke(true.B)
      dut.io.cacheWrite.req.ready.poke(false.B)
      dut.io.csrCtrl.hd_misalign_st_enable.poke(true.B)
      dut.io.tlbCsr.priv.dmode.poke((if (trial.translated) 0 else 3).U)
      dut.io.tlbCsr.satp.mode.poke((if (trial.translated) trial.translationMode else 0).U)
      dut.io.tlbCsr.mPBMTE.poke(trial.highNoncache.B)
      dut.io.tlbCsr.pmm.mseccfg.poke(trial.pmm.U)
      dut.io.tlbCsr.pmm.senvcfg.poke(trial.pmm.U)
      dut.io.pmpEnvironment.cmode.poke(true.B)
      dut.io.pmpEnvironment.mode.poke(0.U)
      for (entries <- Seq(dut.io.pmpEnvironment.pmp, dut.io.pmpEnvironment.pma)) {
        entries.head.cfg.a.poke(1.U); entries.head.addr.poke(0x40000.U)
        entries.head.cfg.r.poke(true.B); entries.head.cfg.w.poke(true.B); entries.head.cfg.x.poke(true.B)
      }
      dut.io.pmpEnvironment.pma.head.cfg.c.poke(true.B)
      if (trial.highProtection != "none") {
        val bank = if (trial.highProtection == "pmp-write") dut.io.pmpEnvironment.pmp else dut.io.pmpEnvironment.pma
        bank.head.addr.poke(0x28000.U)
        bank(1).cfg.a.poke(1.U); bank(1).addr.poke(0x40000.U)
        bank(1).cfg.r.poke(true.B); bank(1).cfg.x.poke(true.B)
        bank(1).cfg.w.poke((trial.highProtection == "pma-noncache").B)
        bank(1).cfg.c.poke((trial.highProtection != "pma-noncache").B)
      }
      dut.io.config.foreach { config =>
        config.sourcePrivilege.poke(0.U); config.policy.uEnable.poke(true.B)
        config.entries.head.entryValid.poke(true.B); config.entries.head.writeAllowed.poke(true.B)
        config.entries.head.boundLo.poke(trial.lowBound.U); config.entries.head.boundHi.poke(trial.highBound.U)
      }
      dut.reset.poke(true.B); dut.clock.step(4)
      dut.reset.poke(false.B)
      (0 until 64).foreach(_ => step())
      dut.io.queueEmpty.expect(true.B); dut.io.bufferEmpty.expect(true.B)
      dut.io.enq.canAccept.expect(true.B)
      zero(dut.io.enq.req.head.bits); dut.io.enq.req.head.bits.numLsElem.poke(1.U)
      dut.io.enq.needAlloc.head.poke(true.B)
      sq = SqIdentity(bool(dut.io.enq.resp.head.flag), uint(dut.io.enq.resp.head.value).toInt)
      driveOwner(dut.io.enq.req.head.bits); dut.io.enq.req.head.valid.poke(true.B)
      event("begin", s"\"enabled\":$enabled,\"address\":${trial.address},\"effective\":${trial.effective},\"sq\":${sq.value},\"expected_cause\":${trial.expectedCause.getOrElse(-1)}")
      step(); allocated = true
      dut.io.enq.req.head.valid.poke(false.B); dut.io.enq.needAlloc.head.poke(false.B)
      if (trial.crossPage && !trial.evictLow) atHead()
      val start = cycle
      var firstAddressAccepted = false
      while (cycle < start + 1200 && (completions == 0 || !bool(dut.io.queueEmpty) || cycle < lastCompletion + 16)) {
        dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
        if (!firstAddressAccepted || retryAt >= 0 && cycle >= retryAt && !waitingAddressFeedback && completions == 0) {
          driveAddress()
        }
        if (!dataAccepted && (trial.dataDelay == 0 || firstIssueCycle >= 0 && cycle >= firstIssueCycle + trial.dataDelay)) {
          val data = dut.io.std(lane)
          zero(data.bits); driveOwner(data.bits.uop)
          data.bits.src(0).poke(trial.data.U); data.bits.isFirstIssue.poke(true.B); data.valid.poke(true.B)
        }
        val acceptsAddress = bool(dut.io.sta(lane).valid) && bool(dut.io.sta(lane).ready)
        if (acceptsAddress) { firstAddressAccepted = true; retryAt = -1 }
        step()
      }
      dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
      assert(allocated && completions == 1 && dataResults == 1, "Original STA/STD owner did not complete once")
      assert(bool(dut.io.queueEmpty), "Store owner did not leave the real SQ")
      assert(dequeues == 1, s"One original SQ entry must dequeue once, saw $dequeues")
      if (trial.expectedCause.nonEmpty) {
        dut.view.exceptionValid.expect(true.B)
        dut.view.exceptionOwner.value.expect(rob.U); dut.view.exceptionOwner.flag.expect(false.B)
        val actualAddress = observedExceptionAddress(dut)
        event("exception-address", s"\"raw\":${uint(dut.view.exceptionAddress)},\"needs_extension\":${bool(dut.view.exceptionNeedsExtension)},\"is_hyper\":${bool(dut.view.exceptionIsHyper)},\"architectural\":$actualAddress,\"expected\":${trial.expectedFaultAddress}")
        assert(actualAddress == trial.expectedFaultAddress,
          s"${trial.name}: architectural store fault address $actualAddress != ${trial.expectedFaultAddress}")
        assert(byteWrites == 0 && publishedBytes.isEmpty && cacheWrites == 0 && uncacheWrites == 0,
          "Exception path modified target data")
        assert(emptyTokens == 1, "An exceptional original owner must release exactly one empty token")
      } else {
        assert(publishedBytes.toMap == trial.expectedBytes, "SQ/Sbuffer publication did not match the original complete store")
        assert(emptyTokens == 0, "Allowed store emitted an empty exception token")
        if (!trial.cross16) {
          assert(splitRequests == 0, "Within-16B store must complete without entering the split request path")
          assert(publications == 1 && byteWrites == 1, "Within-16B store must publish its byte mask once")
        }
      }
      if (trial.translated && trial.crossPage && !trial.highCanonicalFault) {
        assert(highTranslations > 0, "The high page never reached real translation")
      }
      if (trial.evictLow) assert(fenceAt >= 0 && lowRetries > 0,
        "The low fragment did not retry after the real translation eviction")
      if (trial.ptwDelay > 10) assert(highRetries > 1, "Delayed high-page translation did not exercise repeated fragment retries")
      dut.io.flushBuffer.poke(true.B); dut.io.cacheWrite.req.ready.poke(true.B)
      val flushDeadline = cycle + 300
      while (cycle < flushDeadline && (!bool(dut.io.flushDone) || !bool(dut.io.bufferEmpty) || acknowledgements.nonEmpty)) step()
      (0 until 8).foreach(_ => step())
      dut.io.flushDone.expect(true.B); dut.io.bufferEmpty.expect(true.B); dut.view.splitFull.expect(false.B)
      assert(memoryBytes.toMap == trial.expectedBytes, "Cache-visible byte image does not match the original store")
      assert(metadataResponses.forall(_.isEmpty), "Metadata service has an unconsumed reply")
      event("complete", s"\"attempts\":$attempts,\"split_requests\":$splitRequests,\"split_retries\":$splitRetries,\"publications\":$publications,\"empty_tokens\":$emptyTokens,\"data_writes\":$byteWrites,\"cache_writes\":$cacheWrites,\"metadata_queries\":$metadataRequests")
      println(s"L05_CASE_PASS name=${trial.name} enabled=$enabled cycles=$cycle")
    }
  }

  // Fill real Sbuffer entries through accepted stores. Only the external cache
  // sink is stalled; the SQ, SMB and both publication ports retain native wiring.
  private class PressureDriver(dut: FDIStoreMisalignHarness, events: mutable.ArrayBuffer[String]) {
    private case class Source(rob: Int, sq: SqIdentity, address: BigInt, data: BigInt, split: Boolean)
    private case class Offered(rob: Int, address: BigInt, virtualAddress: BigInt,
      data: BigInt, mask: BigInt, sq: SqIdentity, dequeue: Boolean)
    private var cycle = 0
    private val sources = mutable.ArrayBuffer.empty[Source]
    private val addressAccepted = mutable.Set.empty[Int]
    private val dataAccepted = mutable.Set.empty[Int]
    private val completed = mutable.Set.empty[Int]
    private val dataCompleted = mutable.Set.empty[Int]
    private val publicationCounts = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val tokenCounts = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val expectedBytes = mutable.Map.empty[BigInt, Int]
    private val publishedBytes = mutable.Map.empty[BigInt, Int]
    private val memoryBytes = mutable.Map.empty[BigInt, Int]
    private val metadataResponses = Seq.fill(dut.io.metadata.length)(mutable.Queue.empty[Int])
    private val acknowledgements = mutable.Queue.empty[(Int, BigInt)]
    private var heldPublications = Map.empty[(Int, BigInt), Offered]
    private var dequeues = 0
    private var cacheRequests = 0
    private var splitRequests = 0
    private val capacity = dut.StoreBufferSize
    private val lineBytes = dut.io.cacheWrite.req.bits.mask.getWidth
    private val targetAddress = BigInt(0x20000) + lineBytes - 3
    private val targetRob = 10 + capacity
    require(capacity > 1 && capacity + 10 < dut.RobSize / 2)
    require(lineBytes >= 16 && (lineBytes & (lineBytes - 1)) == 0)
    require(BigInt(0x10000) + capacity * lineBytes < 0x20000 && lineBytes * 2 < 4096)

    private def event(kind: String, fields: String = ""): Unit = events +=
      s"{\"case\":\"$pressureScenario\",\"cycle\":$cycle,\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    private def identify(uop: DynInst): Source = {
      val source = sources.find(_.rob == uint(uop.robIdx.value).toInt).getOrElse(
        throw new AssertionError("Store result has no allocated pressure owner"))
      uop.robIdx.flag.expect(false.B)
      uop.sqIdx.flag.expect(source.sq.flag.B); uop.sqIdx.value.expect(source.sq.value.U)
      uop.uopIdx.expect(0.U)
      source
    }
    private def driveOwner(uop: DynInst, source: Source): Unit = {
      zero(uop)
      uop.instr.poke(0x00113023.U); uop.pc.poke((0x4000 + source.rob * 4).U)
      uop.fuType.poke(FuType.stu.U); uop.fuOpType.poke(3.U)
      uop.robIdx.value.poke(source.rob.U)
      uop.sqIdx.flag.poke(source.sq.flag.B); uop.sqIdx.value.poke(source.sq.value.U)
      uop.firstUop.poke(true.B); uop.lastUop.poke(true.B); uop.numLsElem.poke(1.U)
      uop.fdiNotTrusted.foreach(_.poke(true.B))
    }
    private def atHead(source: Source): Unit = {
      dut.io.rob.pendingPtr.value.poke(source.rob.U)
      dut.io.rob.pendingPtrNext.value.poke(source.rob.U)
      dut.io.rob.pendingst.poke(true.B)
    }
    private def offerSnapshot(): Map[(Int, BigInt), Offered] =
      dut.io.publication.filter(port => bool(port.valid)).map { port =>
        val sq = SqIdentity(bool(port.sq.flag), uint(port.sq.value).toInt)
        val source = sources.find(_.sq == sq).getOrElse(
          throw new AssertionError("Publication has no allocated full SQ identity"))
        port.bits.vecValid.expect(true.B)
        val offered = Offered(source.rob, uint(port.bits.addr), uint(port.bits.vaddr),
          uint(port.bits.data), uint(port.bits.mask), sq, bool(port.bits.sqNeedDeq))
        (source.rob -> offered.address) -> offered
      }.toMap
    private def step(): Unit = {
      for ((port, index) <- dut.io.metadata.zipWithIndex) {
        port.resp.valid.poke(metadataResponses(index).headOption.contains(cycle).B)
        if (metadataResponses(index).headOption.contains(cycle)) metadataResponses(index).dequeue()
      }
      dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (acknowledgements.headOption.exists(_._1 == cycle)) {
        val (_, id) = acknowledgements.dequeue()
        dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        dut.io.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
        event("cache-ack", s"\"id\":$id")
      }
      assert(!dut.io.ptw.req.exists(port => bool(port.valid)), "Bare pressure sources must not request PTW service")
      assert(!bool(dut.io.uncache.req.valid), "Cacheable pressure sources offered an uncached write")
      for ((port, index) <- dut.io.metadata.zipWithIndex if bool(port.req.valid) && bool(port.req.ready)) {
        port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
        metadataResponses(index).enqueue(cycle + 2)
      }
      for (port <- dut.io.sta if bool(port.valid) && bool(port.ready)) {
        val source = identify(port.bits.uop)
        assert(addressAccepted.add(source.rob), "Pressure STA was accepted more than once")
      }
      for (port <- dut.io.std if bool(port.valid) && bool(port.ready)) {
        val source = identify(port.bits.uop)
        assert(dataAccepted.add(source.rob), "Pressure STD was accepted more than once")
      }
      for (feedback <- dut.io.feedback if bool(feedback.valid)) {
        feedback.bits.hit.expect(true.B)
      }
      val writebacks = dut.io.wb.toSeq :+ dut.view.splitWriteback
      for (wb <- writebacks if bool(wb.valid)) {
        val source = identify(wb.bits.uop)
        assert(faults(wb.bits.uop.exceptionVec).isEmpty, "Allowed pressure source raised an exception")
        assert(completed.add(source.rob), "Pressure source completed more than once")
        atHead(source)
        event("store-writeback", s"\"rob\":${source.rob}")
      }
      for (wb <- dut.io.dataWb if bool(wb.valid)) {
        val source = identify(wb.bits.uop)
        assert(dataCompleted.add(source.rob), "Pressure STD completed more than once")
        wb.bits.data.expect(source.data.U)
      }
      if (bool(dut.io.splitRequestFire)) {
        val source = identify(dut.io.splitRequest.bits.uop)
        assert(source.split, "Aligned prefill unexpectedly reached the SMB")
        splitRequests += 1
      }
      val offers = offerSnapshot()
      // The FIFO may move an unconsumed suffix to lane zero after its prefix
      // fires. Track fragment identity instead of requiring a fixed lane.
      heldPublications.foreach { case (key, previous) =>
        assert(offers.get(key).contains(previous), "Unaccepted publication changed or disappeared")
      }
      val accepted = mutable.Set.empty[(Int, BigInt)]
      for ((port, lane) <- dut.io.publication.zipWithIndex) {
        assert(bool(dut.io.writes(lane).valid) == (bool(port.fire) && bool(port.bits.vecValid)),
          "Native Sbuffer write enable disagrees with publication acceptance")
        if (bool(port.fire)) {
          val sq = SqIdentity(bool(port.sq.flag), uint(port.sq.value).toInt)
          val source = sources.find(_.sq == sq).get
          val address = uint(port.bits.addr)
          assert(accepted.add(source.rob -> address), "One fragment appeared on two publication lanes")
          val base = address & ~BigInt(15)
          val mask = uint(port.bits.mask)
          assert(mask != 0, "Allowed pressure source published an empty byte mask")
          val prefix = !source.split || base == (source.address & ~BigInt(15))
          port.bits.sqNeedDeq.expect(prefix.B)
          if (prefix) tokenCounts(source.rob) += 1
          publicationCounts(source.rob) += 1
          for (offset <- 0 until port.bits.mask.getWidth if mask.testBit(offset)) {
            val target = base + offset
            val byte = ((uint(port.bits.data) >> (offset * 8)) & 255).toInt
            assert(expectedBytes.get(target).contains(byte), s"Unexpected pressure publication byte at $target")
            assert(!publishedBytes.contains(target), "Pressure source published a byte twice")
            publishedBytes(target) = byte
          }
          event("sq-publication", s"\"rob\":${source.rob},\"lane\":$lane,\"address\":$address,\"mask\":$mask,\"dequeue\":$prefix")
        }
      }
      heldPublications = offers -- accepted
      dequeues += uint(dut.io.queueDequeue).toInt
      val cache = dut.io.cacheWrite.req
      if (bool(cache.valid) && bool(cache.ready)) {
        cache.bits.cmd.expect(MemoryOpConstants.M_XWR)
        val base = uint(cache.bits.addr)
        assert(base % lineBytes == 0)
        val mask = uint(cache.bits.mask)
        for (offset <- 0 until lineBytes if mask.testBit(offset)) {
          val address = base + offset
          val byte = ((uint(cache.bits.data) >> (offset * 8)) & 255).toInt
          assert(expectedBytes.get(address).contains(byte), s"Unexpected pressure cache byte at $address")
          assert(!memoryBytes.contains(address), "Cache accepted a pressure byte twice")
          memoryBytes(address) = byte
        }
        cacheRequests += 1
        acknowledgements.enqueue((cycle + 3, uint(cache.bits.id)))
        event("cache-write", s"\"address\":$base,\"mask\":$mask")
      }
      dut.clock.step(); cycle += 1
    }
    private def until(label: String, limit: Int = 512)(condition: => Boolean): Unit = {
      val deadline = cycle + limit
      while (!condition && cycle < deadline) step()
      assert(condition, s"Pressure timeout: $label")
    }
    private def issue(address: BigInt, split: Boolean): Source = {
      dut.io.enq.canAccept.expect(true.B)
      zero(dut.io.enq.req.head.bits)
      dut.io.enq.req.head.bits.numLsElem.poke(1.U)
      dut.io.enq.needAlloc.head.poke(true.B)
      val sq = SqIdentity(bool(dut.io.enq.resp.head.flag), uint(dut.io.enq.resp.head.value).toInt)
      val source = Source(10 + sources.size, sq, address,
        BigInt("89abcdef76543210", 16) ^ (BigInt(sources.size) << 32), split)
      assert(!sources.exists(_.sq == sq), "Pressure sources unexpectedly reused a full SQ pointer")
      sources += source
      (0 until 8).foreach(offset => expectedBytes(address + offset) = ((source.data >> (8 * offset)) & 255).toInt)
      driveOwner(dut.io.enq.req.head.bits, source)
      dut.io.enq.req.head.valid.poke(true.B)
      step()
      dut.io.enq.req.head.valid.poke(false.B); dut.io.enq.needAlloc.head.poke(false.B)
      val deadline = cycle + 256
      while ((!addressAccepted(source.rob) || !dataAccepted(source.rob)) && cycle < deadline) {
        val sta = dut.io.sta.head
        val std = dut.io.std.head
        zero(sta.bits); driveOwner(sta.bits.uop, source)
        sta.bits.src(0).poke(address.U); sta.bits.src(1).poke(source.data.U)
        sta.bits.isFirstIssue.poke(true.B); sta.valid.poke((!addressAccepted(source.rob)).B)
        zero(std.bits); driveOwner(std.bits.uop, source)
        std.bits.src(0).poke(source.data.U); std.bits.isFirstIssue.poke(true.B)
        std.valid.poke((!dataAccepted(source.rob)).B)
        step()
      }
      dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
      assert(addressAccepted(source.rob) && dataAccepted(source.rob), "Pressure source was not issued")
      until("unique STA and STD completion") { completed(source.rob) && dataCompleted(source.rob) }
      source
    }

    def run(): Unit = {
      zeroInputs(dut.io)
      dut.io.enq.lqCanAccept.poke(true.B)
      dut.io.metadata.foreach(_.req.ready.poke(true.B))
      dut.io.ptw.req.foreach(_.ready.poke(true.B))
      dut.io.uncache.req.ready.poke(true.B)
      dut.io.cacheWrite.req.ready.poke(false.B)
      dut.io.csrCtrl.hd_misalign_st_enable.poke(true.B)
      dut.io.tlbCsr.priv.dmode.poke(3.U)
      dut.io.pmpEnvironment.cmode.poke(true.B)
      dut.io.pmpEnvironment.mode.poke(0.U)
      for (entries <- Seq(dut.io.pmpEnvironment.pmp, dut.io.pmpEnvironment.pma)) {
        entries.head.cfg.a.poke(1.U); entries.head.addr.poke(0x40000.U)
        entries.head.cfg.r.poke(true.B); entries.head.cfg.w.poke(true.B); entries.head.cfg.x.poke(true.B)
      }
      dut.io.pmpEnvironment.pma.head.cfg.c.poke(true.B)
      dut.io.config.foreach { config =>
        config.sourcePrivilege.poke(0.U); config.policy.uEnable.poke(true.B)
        config.entries.head.entryValid.poke(true.B); config.entries.head.writeAllowed.poke(true.B)
        config.entries.head.boundLo.poke(0x10000.U); config.entries.head.boundHi.poke(0x30000.U)
      }
      dut.reset.poke(true.B); dut.clock.step(4)
      dut.reset.poke(false.B)
      (0 until 64).foreach(_ => step())
      event("begin", s"\"enabled\":$enabled,\"capacity\":$capacity,\"line_bytes\":$lineBytes")
      for (index <- 0 until capacity) {
        val source = issue(BigInt(0x10000) + index * lineBytes, split = false)
        until("prefill publication and SQ release") {
          publicationCounts(source.rob) == 1 && bool(dut.io.queueEmpty) && dequeues == index + 1
        }
      }
      (0 until 4).foreach(_ => step())
      assert(dut.io.bufferState.forall(state => bool(state.state_valid)), "Prefill did not occupy every real Sbuffer entry")
      assert(cacheRequests == 0 && memoryBytes.isEmpty, "Blocked external cache accepted a prefill write")
      event("sbuffer-filled", s"\"occupied\":$capacity,\"sources\":${sources.size}")
      val target = issue(targetAddress, split = true)
      assert(target.rob == targetRob)
      until("both fragments offered against the full Sbuffer") {
        dut.io.publication.forall(port => bool(port.valid) && !bool(port.ready))
      }
      for (_ <- 0 until 12) {
        val pending = offerSnapshot()
        assert(pending.size == 2 && pending.values.forall(_.rob == targetRob),
          "Full-capacity stall did not retain both target fragments")
        dut.io.publication.foreach { port => port.valid.expect(true.B); port.ready.expect(false.B) }
        step()
      }
      assert(publicationCounts(targetRob) == 0 && dequeues == capacity)
      event("dual-publication-held", "\"cycles\":12")
      // Free exactly one entry first, proving that the accepted low prefix does
      // not discard or alter the high suffix while capacity is exhausted again.
      dut.io.cacheWrite.req.ready.poke(true.B)
      until("one external cache acceptance") { cacheRequests == 1 }
      dut.io.cacheWrite.req.ready.poke(false.B)
      until("low fragment consumes the single released entry") { publicationCounts(targetRob) == 1 }
      assert(tokenCounts(targetRob) == 1)
      for (_ <- 0 until 6) {
        val pending = offerSnapshot()
        assert(pending.size == 1 && pending.values.forall(_.rob == targetRob),
          "The remaining high fragment was not retained")
        dut.io.publication.head.ready.expect(false.B)
        assert(publicationCounts(targetRob) == 1)
        step()
      }
      event("high-suffix-held", "\"cycles\":6")
      dut.io.cacheWrite.req.ready.poke(true.B)
      until("both fragments accepted and one original entry released") {
        publicationCounts(targetRob) == 2 && bool(dut.io.queueEmpty) && dequeues == capacity + 1
      }
      assert(splitRequests == 2, "The bare split did not execute exactly two real STA fragments")
      assert(sources.forall(source => tokenCounts(source.rob) == 1), "A source consumed the SQ dequeue token more than once")
      assert(publishedBytes.toMap == expectedBytes.toMap, "Pressure publication lost or changed source bytes")
      dut.io.flushBuffer.poke(true.B)
      until("Sbuffer and outstanding cache replies drained", 1024) {
        bool(dut.io.flushDone) && bool(dut.io.bufferEmpty) && acknowledgements.isEmpty
      }
      (0 until 16).foreach(_ => step())
      assert(memoryBytes.toMap == expectedBytes.toMap, "Pressure cache image lost or changed source bytes")
      assert(heldPublications.isEmpty && metadataResponses.forall(_.isEmpty))
      assert(completed.size == sources.size && dataCompleted.size == sources.size && dequeues == sources.size)
      dut.view.splitFull.expect(false.B)
      event("complete", s"\"sources\":${sources.size},\"dequeues\":$dequeues,\"cache_requests\":$cacheRequests,\"bytes\":${memoryBytes.size}")
      println(s"L05_CASE_PASS name=$pressureScenario enabled=$enabled cycles=$cycle")
    }
  }

  // Redirect only an allocated, uncommitted owner. A replacement is allocated
  // after the native SQ cancellation count and pointer recovery, without reset.
  private class CancellationDriver(dut: FDIStoreMisalignHarness, name: String,
    events: mutable.ArrayBuffer[String]) {
    private case class Source(rob: Int, sq: SqIdentity, address: BigInt, data: BigInt)
    private case class Reply(due: Int, vpn: BigInt, source: Source)
    private val cancelProbe = name == "cancel-probe-s2-new-owner"
    private val cancelTerminalS1 = name == "cancel-terminal-s1-new-owner"
    private val cancelTerminalS2 = name == "cancel-terminal-s2-new-owner"
    private val cancelTerminal = cancelTerminalS1 || cancelTerminalS2
    private val deferred = cancelProbe || cancelTerminal
    private val translated = name == "cancel-high-fragment-s2-new-owner" || deferred
    private val cancelLatency = if (translated && !cancelTerminalS1) 2 else 1
    require(!deferred || enabled, "Deferred store cancellation requires the feature")
    private val oldAddress = BigInt(if (translated) 0x8ffd else 0x800d)
    private val newAddress = BigInt(0xa000)
    private var cycle = 0
    private var oldSource: Option[Source] = None
    private var newSource: Option[Source] = None
    private var cancelAt = -1
    private var cancellationOrigin = -1
    private var cancelCount = 0
    private var cancelCountSampled = false
    private var dequeueCount = 0
    private var lateOldPtw = 0
    private var replacementCompleted = false
    private var replacementCompletionCycle = -1
    private var replacementPublished = 0
    private var candidateEnqueues = 0
    private var oldHighRequests = 0
    private var probeRequests = 0
    private var terminalRequests = 0
    private var terminalPrimaries = 0
    private var terminalPrimaryCycle = -1
    private val attempts = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val dataAccepted = mutable.Set.empty[Int]
    private val dataResults = mutable.Set.empty[Int]
    private val waitingFeedback = mutable.Set.empty[Int]
    private val retryAt = mutable.Map.empty[Int, Int]
    private val metadataResponses = Seq.fill(dut.io.metadata.length)(mutable.Queue.empty[Int])
    private val acknowledgements = mutable.Queue.empty[(Int, BigInt)]
    private val ptwReplies = mutable.ArrayBuffer.empty[Reply]
    private val scheduledVpns = mutable.Set.empty[BigInt]
    private val publishedBytes = mutable.Map.empty[BigInt, Int]
    private val memoryBytes = mutable.Map.empty[BigInt, Int]
    private val newData = BigInt("1020304050607080", 16)
    private val newPhysical = if (translated) BigInt(0xc0000) else newAddress
    private val expectedBytes = (0 until 8).map(offset =>
      (newPhysical + offset) -> ((newData >> (offset * 8)) & 255).toInt).toMap

    private def event(kind: String, fields: String = ""): Unit = events +=
      s"{\"case\":\"$name\",\"cycle\":$cycle,\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    private def identify(uop: DynInst): Source = {
      val source = (oldSource.toSeq ++ newSource.toSeq).find(_.rob == uint(uop.robIdx.value).toInt).getOrElse(
        throw new AssertionError("Canceled-store fixture observed an unknown ROB owner"))
      uop.robIdx.flag.expect(false.B)
      uop.sqIdx.flag.expect(source.sq.flag.B); uop.sqIdx.value.expect(source.sq.value.U)
      uop.uopIdx.expect(0.U)
      source
    }
    private def driveOwner(uop: DynInst, source: Source): Unit = {
      zero(uop)
      uop.instr.poke(0x00113023.U); uop.pc.poke((0x4000 + source.rob * 4).U)
      uop.fuType.poke(FuType.stu.U); uop.fuOpType.poke(3.U)
      uop.robIdx.value.poke(source.rob.U)
      uop.sqIdx.flag.poke(source.sq.flag.B); uop.sqIdx.value.poke(source.sq.value.U)
      uop.firstUop.poke(true.B); uop.lastUop.poke(true.B); uop.numLsElem.poke(1.U)
      uop.fdiNotTrusted.foreach(_.poke(true.B))
    }
    private def atHead(source: Source): Unit = {
      dut.io.rob.pendingPtr.value.poke(source.rob.U)
      dut.io.rob.pendingPtrNext.value.poke(source.rob.U)
      dut.io.rob.pendingst.poke(true.B)
    }
    private def driveIssue(source: Source): Unit = {
      val sta = dut.io.sta.head
      val std = dut.io.std.head
      val retryReady = retryAt.get(source.rob).exists(_ <= cycle) && !waitingFeedback(source.rob)
      if (attempts(source.rob) == 0 || retryReady) {
        zero(sta.bits); driveOwner(sta.bits.uop, source)
        sta.bits.src(0).poke(source.address.U); sta.bits.src(1).poke(source.data.U)
        sta.bits.isFirstIssue.poke((attempts(source.rob) == 0).B); sta.valid.poke(true.B)
      }
      if (!dataAccepted(source.rob)) {
        zero(std.bits); driveOwner(std.bits.uop, source)
        std.bits.src(0).poke(source.data.U); std.bits.isFirstIssue.poke(true.B); std.valid.poke(true.B)
      }
    }
    private def receiveCompletion(wb: MemExuOutput, path: String): Unit = {
      val source = identify(wb.uop)
      if (source.rob == 10) {
        assert(!deferred, "Canceled deferred owner published a final store writeback")
        // The native interface may expose its current result on the redirect
        // cycle. That canceled sample never counts as a surviving completion.
        assert(cycle == cancelAt, "Canceled store published a writeback outside its redirect cycle")
        event("redirect-cycle-writeback", s"\"path\":\"$path\",\"rob\":10")
      } else {
        assert(!replacementCompleted && faults(wb.uop.exceptionVec).isEmpty,
          "Replacement store duplicated completion or inherited an exception")
        replacementCompleted = true
        replacementCompletionCycle = cycle
        atHead(source)
        event("replacement-writeback", "\"rob\":12")
      }
    }
    private def step(): Unit = {
      dut.io.redirect.valid.poke(false.B)
      for ((port, index) <- dut.io.metadata.zipWithIndex) {
        port.resp.valid.poke(metadataResponses(index).headOption.contains(cycle).B)
        if (metadataResponses(index).headOption.contains(cycle)) metadataResponses(index).dequeue()
      }
      dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (acknowledgements.headOption.exists(_._1 == cycle)) {
        val (_, id) = acknowledgements.dequeue()
        dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        dut.io.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
      }
      dut.io.ptw.resp.valid.poke(false.B)
      ptwReplies.find(_.due == cycle).foreach { reply =>
        val response = dut.io.ptw.resp.bits
        zero(response)
        val sector = response.s1.pteidx.length
        val index = (reply.vpn % sector).toInt
        val ppn = if (reply.vpn == 8) BigInt(0x80) else if (reply.vpn == 9) BigInt(0xa0) else BigInt(0xc0)
        response.memidx.is_st.poke(true.B); response.memidx.idx.poke(reply.source.sq.value.U)
        response.s1.entry.tag.poke((reply.vpn / sector).U)
        response.s1.entry.level.foreach(_.poke(0.U))
        response.s1.entry.ppn.poke((ppn / sector).U); response.s1.entry.v.poke(true.B)
        response.s1.entry.perm.foreach { perm =>
          perm.r.poke(true.B); perm.w.poke(true.B); perm.a.poke(true.B); perm.d.poke(true.B); perm.u.poke(true.B)
        }
        response.s1.addr_low.poke(index.U)
        response.s1.valididx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.pteidx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.ppn_low.zipWithIndex.foreach { case (part, i) => part.poke((if (i == index) ppn % sector else BigInt(i)).U) }
        dut.io.ptw.resp.valid.poke(true.B); dut.io.ptw.resp.ready.expect(true.B)
        if (reply.source.rob == 10 && cancelAt >= 0 && cycle > cancelAt) {
          assert(newSource.nonEmpty && attempts(12) > 0 && !replacementCompleted,
            "Old PTW response did not overlap the live replacement transaction")
          lateOldPtw += 1
          event("late-old-ptw-response", s"\"vpn\":${reply.vpn},\"old_sq\":${reply.source.sq.value},\"new_sq\":${newSource.get.sq.value}")
        }
      }
      if (cycle == cancelAt) {
        val source = oldSource.get
        val slot = dut.io.slots(source.sq.value)
        // Check the public pre-redirect state; never manufacture rollback of a
        // committed store merely to make the cancellation test progress.
        val awaitingS2 = bool(slot.waitS2)
        assert(bool(slot.allocated) && !bool(slot.committed),
          "Requested cancellation window was not allocated and uncommitted")
        if (translated) assert(awaitingS2,
          "Internal store cancellation did not precede final Store S2 acceptance")
        slot.rob.flag.expect(false.B); slot.rob.value.expect(10.U)
        slot.sq.flag.expect(source.sq.flag.B); slot.sq.value.expect(source.sq.value.U)
        if (name == "cancel-sta-s1-new-owner" || cancelTerminalS1) {
          dut.io.phases.head.s1.expect(true.B)
          dut.io.phases.head.source.flag.expect(false.B)
          dut.io.phases.head.source.value.expect(10.U)
        } else {
          dut.io.phases.head.s2.expect(true.B)
          assert(identify(dut.io.phases.head.supplement.uop).rob == 10,
            "Cancellation stage belongs to another full ROB/SQ identity")
        }
        if (cancelTerminalS2) assert(terminalPrimaries == 1 && cycle == terminalPrimaryCycle + 1,
          "Terminal S2 cancellation must follow one accepted matching T1 primary")
        dut.io.redirect.bits.robIdx.flag.poke(false.B); dut.io.redirect.bits.robIdx.value.poke(10.U)
        dut.io.redirect.bits.level.poke(RedirectLevel.flush)
        dut.io.redirect.valid.poke(true.B)
        event("redirect", s"\"rob\":10,\"sq\":${source.sq.value},\"committed\":false,\"wait_store_s2\":$awaitingS2,\"deferred\":$deferred")
      }
      assert(!bool(dut.io.uncache.req.valid), "Canceled or replacement store offered an uncached target write")
      for ((port, index) <- dut.io.metadata.zipWithIndex if bool(port.req.valid) && bool(port.req.ready)) {
        port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
        metadataResponses(index).enqueue(cycle + 2)
      }
      for (request <- dut.io.ptw.req if bool(request.valid) && bool(request.ready)) {
        val vpn = uint(request.bits.vpn)
        assert(translated && Set(BigInt(8), BigInt(9), BigInt(10))(vpn), "Cancellation PTW request escaped its three pages")
        val source = if (vpn == 10) newSource.get else oldSource.get
        request.bits.memidx.is_st.expect(true.B); request.bits.memidx.idx.expect(source.sq.value.U)
        if (scheduledVpns.add(vpn)) {
          val delay = if (source.rob == 12) 24 else if (vpn == 9) 20 else 8
          ptwReplies += Reply(cycle + delay, vpn, source)
        }
        event("ptw-request", s"\"rob\":${source.rob},\"vpn\":$vpn")
      }
      for (port <- dut.io.sta if bool(port.valid) && bool(port.ready)) {
        val source = identify(port.bits.uop)
        attempts(source.rob) += 1; waitingFeedback += source.rob; retryAt -= source.rob
        if (source.rob == 10 && name == "cancel-sta-s1-new-owner" && cancelAt < 0) {
          cancellationOrigin = cycle; cancelAt = cycle + 1
        }
        event("original-sta", s"\"rob\":${source.rob},\"attempt\":${attempts(source.rob)}")
      }
      for (port <- dut.io.std if bool(port.valid) && bool(port.ready)) {
        val source = identify(port.bits.uop)
        assert(dataAccepted.add(source.rob), "Cancellation source accepted STD twice")
      }
      for (feedback <- dut.io.feedback if bool(feedback.valid)) {
        val rob = uint(feedback.bits.robIdx.value).toInt
        assert(rob == 10 || rob == 12)
        waitingFeedback -= rob
        if (!bool(feedback.bits.hit) && !(rob == 10 && cancelAt >= 0)) retryAt(rob) = cycle + 30
      }
      for (enq <- dut.view.splitEnqueue if bool(enq.valid)) {
        val source = identify(enq.bits.uop)
        if (source.rob == 10) {
          candidateEnqueues += 1
          if (name == "cancel-smb-candidate-s2-new-owner" && cancelAt < 0) {
            cancellationOrigin = cycle; cancelAt = cycle + 1
          }
        } else assert(false, "Aligned replacement unexpectedly entered the SMB")
      }
      if (bool(dut.io.splitRequestFire)) {
        val request = dut.io.splitRequest.bits
        val source = identify(request.uop)
        assert(source.rob == 10 && (cancelAt < 0 || cycle < cancelAt), "Canceled owner issued a new split request")
        val probe = request.fdiStoreProbe.exists(bool)
        val terminal = bool(request.isFrmMisAlignBuf) && bool(request.hasException)
        assert(!(probe && terminal), "Store probe and terminal markers overlap")
        if (deferred) {
          assert(probe || terminal, "Denied source issued an ordinary data fragment")
          request.uop.robIdx.value.expect(10.U)
          if (probe) {
            probeRequests += 1
            request.isFinalSplit.expect(true.B)
            if (cancelProbe && cancelAt < 0) { cancellationOrigin = cycle; cancelAt = cycle + cancelLatency }
          }
          if (terminal) {
            terminalRequests += 1
            assert(cancelTerminal && terminalRequests == 1, "Unexpected or duplicate deferred terminal")
            request.fullva.expect(oldAddress.U)
            assert(faults(request.uop.exceptionVec) == Set(24), "Pure denied terminal changed its selected fault")
            if (cancelAt < 0) {
              cancellationOrigin = cycle
              if (!cancelTerminalS2) cancelAt = cycle + cancelLatency
            }
          }
        }
        if (bool(request.isFinalSplit)) {
          oldHighRequests += 1
          if (translated && !deferred && cancelAt < 0) {
            cancellationOrigin = cycle; cancelAt = cycle + cancelLatency
          }
        }
        event("internal-request", s"\"rob\":10,\"probe\":$probe,\"terminal\":$terminal,\"fullva\":${uint(request.fullva)}")
      }
      if (deferred) {
        for (phase <- dut.io.phases if bool(phase.primary.valid) &&
          uint(phase.primary.bits.uop.robIdx.value) == 10) {
          if (bool(phase.primary.bits.hasException)) {
            assert(cancelTerminalS2 && terminalRequests == 1 && terminalPrimaries == 0 &&
              cancelAt < 0 && cycle == cancellationOrigin + 1,
              "Canceled deferred owner published an unexpected terminal SQ primary")
            val source = identify(phase.primary.bits.uop)
            val slot = dut.io.slots(source.sq.value)
            phase.s1.expect(true.B)
            phase.primary.bits.isFrmMisAlignBuf.expect(true.B)
            phase.primary.bits.miss.expect(false.B)
            phase.primary.bits.updateAddrValid.expect(true.B)
            phase.primary.bits.fullva.expect(oldAddress.U)
            assert(faults(phase.primary.bits.uop.exceptionVec) == Set(24))
            assert(bool(slot.allocated) && !bool(slot.committed) && bool(slot.waitS2))
            slot.rob.flag.expect(false.B); slot.rob.value.expect(source.rob.U)
            slot.sq.flag.expect(source.sq.flag.B); slot.sq.value.expect(source.sq.value.U)
            assert(!bool(dut.io.redirect.valid) && !dut.io.enq.req.exists(port => bool(port.valid)),
              "Terminal primary acceptance requires a live owner without redirect or reallocation")
            // The Valid SQ port accepts this matching live terminal at T1; the production
            // terminalOwnerLive assertion also checks its complete saved uop identity.
            terminalPrimaries += 1; terminalPrimaryCycle = cycle
            cancelAt = cycle + 1
            event("terminal-primary-accepted", s"\"rob\":10,\"sq\":${source.sq.value},\"sq_flag\":${source.sq.flag}")
          }
        }
        if (cancelTerminalS2 && cycle == cancelAt) {
          val phase = dut.io.phases.head
          phase.s2.expect(false.B)
          phase.supplement.updateAddrValid.expect(false.B)
          phase.supplement.hasException.expect(false.B)
          event("terminal-s2-canceled", "\"final_supplement\":false")
        }
        if (bool(dut.view.splitResponse.valid) && uint(dut.view.splitResponse.bits.uop.robIdx.value) == 10) {
          assert(!bool(dut.view.splitResponse.bits.hasException), "Canceled deferred owner emitted a final S2 acknowledgement")
        }
        if (cancelAt >= 0 && cycle >= cancelAt && bool(dut.view.splitFull)) {
          // Redirect may still expose the pre-edge saved owner; later it must be gone.
          if (cycle > cancelAt) assert(uint(dut.view.splitOwner.value) != 10,
            "Canceled deferred owner remained in the SMB")
        }
      }
      for (wb <- dut.io.wb if bool(wb.valid)) receiveCompletion(wb.bits, "sta")
      if (bool(dut.view.splitWriteback.valid)) receiveCompletion(dut.view.splitWriteback.bits, "smb")
      for (wb <- dut.io.dataWb if bool(wb.valid)) {
        val source = identify(wb.bits.uop)
        if (source.rob == 10 && cycle == cancelAt) event("redirect-cycle-std-writeback")
        else {
          assert(source.rob != 10 || cancelAt < 0 || cycle < cancelAt, "Canceled STD published a later result")
          assert(dataResults.add(source.rob), "Cancellation STD completed more than once")
          wb.bits.data.expect(source.data.U)
        }
      }
      for ((port, index) <- dut.io.publication.zipWithIndex) {
        assert(bool(dut.io.writes(index).valid) == (bool(port.fire) && bool(port.bits.vecValid)))
        if (bool(port.fire)) {
          assert(newSource.nonEmpty && replacementCompleted, "Old canceled store reached SQ publication")
          val source = newSource.get
          port.sq.flag.expect(source.sq.flag.B); port.sq.value.expect(source.sq.value.U)
          port.bits.vecValid.expect(true.B); port.bits.sqNeedDeq.expect(true.B)
          assert(replacementPublished == 0, "Replacement published more than once")
          replacementPublished += 1
          val base = uint(port.bits.addr) & ~BigInt(15)
          val mask = uint(port.bits.mask)
          for (offset <- 0 until port.bits.mask.getWidth if mask.testBit(offset)) {
            val address = base + offset
            val byte = ((uint(port.bits.data) >> (offset * 8)) & 255).toInt
            assert(expectedBytes.get(address).contains(byte) && !publishedBytes.contains(address),
              "Canceled source or duplicate byte contaminated replacement publication")
            publishedBytes(address) = byte
          }
        }
      }
      // SQ holds its cancellation count until another redirect; its consumer samples it at T2.
      if (cancelAt >= 0 && cycle == cancelAt + 2) {
        assert(!cancelCountSampled, "One redirect cancellation count was sampled twice")
        cancelCount = uint(dut.io.queueCancel).toInt
        cancelCountSampled = true
        event("cancel-count", s"\"count\":$cancelCount,\"redirect_cycle\":$cancelAt")
      }
      dequeueCount += uint(dut.io.queueDequeue).toInt
      val cache = dut.io.cacheWrite.req
      if (bool(cache.valid) && bool(cache.ready)) {
        cache.bits.cmd.expect(MemoryOpConstants.M_XWR)
        val mask = uint(cache.bits.mask)
        val base = uint(cache.bits.addr)
        for (offset <- 0 until cache.bits.mask.getWidth if mask.testBit(offset)) {
          val address = base + offset
          val byte = ((uint(cache.bits.data) >> (offset * 8)) & 255).toInt
          assert(expectedBytes.get(address).contains(byte) && !memoryBytes.contains(address),
            "Canceled source or duplicate byte reached cache")
          memoryBytes(address) = byte
        }
        acknowledgements.enqueue((cycle + 3, uint(cache.bits.id)))
      }
      dut.clock.step(); cycle += 1
    }
    private def allocate(rob: Int, address: BigInt, data: BigInt): Source = {
      dut.io.enq.canAccept.expect(true.B)
      zero(dut.io.enq.req.head.bits); dut.io.enq.req.head.bits.numLsElem.poke(1.U)
      dut.io.enq.needAlloc.head.poke(true.B)
      val source = Source(rob, SqIdentity(bool(dut.io.enq.resp.head.flag), uint(dut.io.enq.resp.head.value).toInt), address, data)
      if (rob == 10) oldSource = Some(source) else newSource = Some(source)
      driveOwner(dut.io.enq.req.head.bits, source); dut.io.enq.req.head.valid.poke(true.B)
      event("allocate", s"\"rob\":$rob,\"sq\":${source.sq.value},\"sq_flag\":${source.sq.flag}")
      step()
      dut.io.enq.req.head.valid.poke(false.B); dut.io.enq.needAlloc.head.poke(false.B)
      source
    }
    def run(): Unit = {
      zeroInputs(dut.io)
      dut.io.enq.lqCanAccept.poke(true.B)
      dut.io.metadata.foreach(_.req.ready.poke(true.B)); dut.io.ptw.req.foreach(_.ready.poke(true.B))
      dut.io.uncache.req.ready.poke(true.B); dut.io.cacheWrite.req.ready.poke(false.B)
      dut.io.csrCtrl.hd_misalign_st_enable.poke(true.B)
      dut.io.tlbCsr.priv.dmode.poke((if (translated) 0 else 3).U)
      dut.io.tlbCsr.satp.mode.poke((if (translated) 8 else 0).U)
      dut.io.pmpEnvironment.cmode.poke(true.B); dut.io.pmpEnvironment.mode.poke(0.U)
      for (entries <- Seq(dut.io.pmpEnvironment.pmp, dut.io.pmpEnvironment.pma)) {
        entries.head.cfg.a.poke(1.U); entries.head.addr.poke(0x40000.U)
        entries.head.cfg.r.poke(true.B); entries.head.cfg.w.poke(true.B); entries.head.cfg.x.poke(true.B)
      }
      dut.io.pmpEnvironment.pma.head.cfg.c.poke(true.B)
      dut.io.config.foreach { config =>
        config.sourcePrivilege.poke(0.U); config.policy.uEnable.poke(true.B)
        config.entries.head.entryValid.poke(true.B); config.entries.head.writeAllowed.poke(true.B)
        config.entries.head.boundLo.poke(0x8000.U)
        config.entries.head.boundHi.poke((if (deferred) 0x9000 else 0xb000).U)
        if (deferred) {
          config.entries(1).entryValid.poke(true.B); config.entries(1).writeAllowed.poke(true.B)
          config.entries(1).boundLo.poke(0xa000.U); config.entries(1).boundHi.poke(0xb000.U)
        }
      }
      dut.reset.poke(true.B); dut.clock.step(4)
      dut.reset.poke(false.B)
      (0 until 64).foreach(_ => step())
      event("begin", s"\"enabled\":$enabled,\"translated\":$translated")
      val old = allocate(10, oldAddress, BigInt("89abcdef76543210", 16))
      if (translated) atHead(old)
      val cancelDeadline = cycle + 512
      while ((cancelAt < 0 || cycle <= cancelAt) && cycle < cancelDeadline) {
        dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
        if (cancelAt < 0 || cycle < cancelAt) driveIssue(old)
        step()
      }
      dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
      assert(cancelAt >= 0 && cycle > cancelAt, "No accepted request reached the selected cancellation boundary")
      val recoveryDeadline = cycle + 32
      while ((!cancelCountSampled || !bool(dut.io.queueEmpty) || !bool(dut.io.enq.canAccept) ||
        cycle < cancelAt + 4) && cycle < recoveryDeadline) step()
      assert(cancelCountSampled && cancelCount == 1 && bool(dut.io.queueEmpty) && bool(dut.io.enq.canAccept),
        "Native SQ cancellation did not recover one entry")
      val replacement = allocate(12, newAddress, newData)
      if (cancelTerminalS2) assert(replacement.sq == old.sq,
        "Terminal cancellation replacement must exercise the recovered original SQ slot")
      val completionDeadline = cycle + 512
      while ((!replacementCompleted || !bool(dut.io.queueEmpty) || cycle < replacementCompletionCycle + 24 ||
        ptwReplies.exists(_.due >= cycle)) && cycle < completionDeadline) {
        dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
        if (!replacementCompleted) driveIssue(replacement)
        step()
      }
      dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
      assert(replacementCompleted && dataResults(12) && replacementPublished == 1 && bool(dut.io.queueEmpty),
        "Replacement store did not progress after cancellation")
      assert(cancelCount == 1 && dequeueCount == 1 && publishedBytes.toMap == expectedBytes)
      assert(cancellationOrigin >= 0 && cancelAt == cancellationOrigin + cancelLatency)
      if (name == "cancel-smb-candidate-s2-new-owner") assert(candidateEnqueues == 1)
      if (translated) assert(oldHighRequests > 0, "Cancellation never accepted a high-fragment request")
      if (translated && !cancelTerminal) assert(lateOldPtw > 0,
        "High-fragment cancellation did not retain a real late PTW response alongside the replacement")
      if (deferred) assert(probeRequests > 0 && terminalRequests == (if (cancelTerminal) 1 else 0),
        "Deferred cancellation did not reach its designated probe or terminal")
      if (cancelTerminalS2) assert(terminalPrimaries == 1 && terminalPrimaryCycle == cancellationOrigin + 1)
      dut.view.splitFull.expect(false.B)
      if (bool(dut.view.exceptionValid)) assert(uint(dut.view.exceptionOwner.value) != 10,
        "Canceled owner remained in the exception buffer")
      dut.io.flushBuffer.poke(true.B); dut.io.cacheWrite.req.ready.poke(true.B)
      val flushDeadline = cycle + 512
      while ((!bool(dut.io.flushDone) || !bool(dut.io.bufferEmpty) || acknowledgements.nonEmpty) && cycle < flushDeadline) step()
      (0 until 16).foreach(_ => step())
      dut.io.flushDone.expect(true.B); dut.io.bufferEmpty.expect(true.B)
      assert(memoryBytes.toMap == expectedBytes && metadataResponses.forall(_.isEmpty),
        "Replacement cache image or service drainage was incorrect")
      event("complete", s"\"cancel_count\":$cancelCount,\"dequeue_count\":$dequeueCount,\"late_old_ptw\":$lateOldPtw,\"probe_requests\":$probeRequests,\"terminal_requests\":$terminalRequests,\"terminal_primaries\":$terminalPrimaries")
      println(s"L05_CASE_PASS name=$name enabled=$enabled cycles=$cycle")
    }
  }

  it should "preserve whole-access faults and publish only accepted store bytes" in {
    val root = Paths.get(sys.props.getOrElse("l05.runRoot", ".")).toAbsolutePath
    Files.createDirectories(root)
    require(Paths.get("").toRealPath() == root.toRealPath(), "Run from the dedicated store-misalignment directory")
    val (base, _, _) = top.ArgParser.parse(Array("--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
      "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
    }
    require(p(XSCoreParamsKey).HasVPU && p(XSCoreParamsKey).VLEN == 128)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val selected = if (scenario == "all" || scenario == "acceptance") cases
      else groups.getOrElse(scenario, cases.filter(_.name == scenario))
    val runPressure = scenario == pressureScenario || scenario == "acceptance"
    val selectedCancellations = if (scenario == "acceptance" || scenario == "supplemental") cancellationScenarios ++ deferredCancellationScenarios
      else if (scenario == "cancellation") cancellationScenarios
      else if (scenario == "deferred-cancellation") deferredCancellationScenarios
      else (cancellationScenarios ++ deferredCancellationScenarios).filter(_ == scenario)
    val events = mutable.ArrayBuffer.empty[String]
    val failures = mutable.ArrayBuffer.empty[String]
    val path = root.resolve(s"store-misalign-$scenario-$enabled")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    try {
      val module = workspace.elaborateGeneratedModule(() => new FDIStoreMisalignHarness)
      workspace.generateAdditionalSources()
      val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
      simulation.runElaboratedModule(module) { elaborated =>
        val dut = elaborated.wrapped
        selected.foreach { trial =>
          try new Driver(dut, trial, events).run()
          catch {
            case NonFatal(error) if error.isInstanceOf[org.scalatest.exceptions.TestFailedException] =>
              val description = Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
              failures += s"${trial.name}: $description"
              val escaped = description.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")
              events += s"{\"case\":\"${trial.name}\",\"event\":\"case-failed\",\"error\":\"$escaped\"}"
              println(s"L05_CASE_FAIL name=${trial.name} enabled=$enabled error=$description")
            case NonFatal(error) =>
              // A terminated simulator cannot be repaired by resetting DUT inputs.
              events += s"{\"case\":\"${trial.name}\",\"event\":\"simulation-aborted\",\"exception\":\"${error.getClass.getSimpleName}\"}"
              throw error
          }
        }
        if (runPressure) new PressureDriver(dut, events).run()
        selectedCancellations.foreach(name => new CancellationDriver(dut, name, events).run())
      }
      assert(failures.isEmpty, failures.mkString("Store cases failed: ", " | ", ""))
      println(s"L05_MISALIGN_PASS enabled=$enabled scenario=$scenario cases=${(if (runPressure) 1 else 0) + selected.size + selectedCancellations.size}")
    } finally {
      Files.write(root.resolve("store-misalign-events.jsonl"), events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
  }
}
