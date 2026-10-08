// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import xiangshan._
import xiangshan.backend.fu.FuType

class FDILoadPermissionTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Ordinary load permission in the production load and translation pipeline"

  private val enabled = sys.env.getOrElse("L01_FDI_ENABLED",
    throw new IllegalArgumentException("L01_FDI_ENABLED must be explicit")).toBoolean
  private val scenario = sys.env.getOrElse("L01_SCENARIO", "minimal")
  require(Set("minimal", "full", "remaining").contains(scenario))

  private def zero(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(zero)
    case value: Record => value.elements.values.foreach(zero)
    case other => throw new IllegalArgumentException(s"Unsupported test input: ${other.getClass}")
  }
  private def uint(data: UInt): BigInt = data.peek().litValue
  private def bool(data: Bool): Boolean = data.peek().litToBoolean

  // These instruction/address/data facts are chosen before observing any DUT result.
  private case class Source(rob: Int, address: BigInt, data: BigInt, flag: Boolean = false) {
    val size = 8
    val allow: Boolean = address >= 0x8000 && address + size <= 0x8008
  }
  private case class CacheAttempt(source: Source, accepted: Int, id: BigInt)

  private val wordMask = (BigInt(1) << 64) - 1
  private val pattern = BigInt("f1e2d3c4b5a69788776655443322ff80", 16)
  private val sbufferPattern = BigInt("e172639485a6b7c80f1e2d3c4b5a6978", 16)
  private val mshrPattern = BigInt("b126d34e58fa907c813649a2c5e70bdf", 16)
  private val channelPattern = BigInt("ca11beef98765432fedc1029384756abb4c3d2e1f08796a523416789abcdef05", 16)
  private case class Region(lo: BigInt, hi: BigInt, valid: Boolean = true, read: Boolean = true)
  private case class Trial(name: String, base: BigInt = 0x8000, immediate: Int = 0,
    operation: Int = 3, fp: Boolean = false, privilege: Int = 0, virtual: Boolean = false,
    active: Boolean = true, trusted: Boolean = false, close: Boolean = false,
    regions: Seq[Region] = Seq(Region(0x8000, 0x8010)), satp: Int = 0, pmm: Int = 0,
    pageFault: Boolean = false, pmpFault: Boolean = false, accessFault: Boolean = false, stalePmp: Boolean = false,
    lateError: Boolean = false, tagError: Boolean = false, fastReplay: Boolean = false,
    normalReplay: Boolean = false, forwardingNoise: Boolean = false,
    mmio: Boolean = false, splitControl: Boolean = false, forwardBytes: Boolean = false,
    cancelAfterMiss: Boolean = false, sbufferBytes: Boolean = false,
    memoryService: String = "none", forwardCorrupt: Boolean = false,
    rob: Int = 2, guestPageFault: Boolean = false, preserveTlb: Boolean = false,
    breakpoint: Boolean = false, pbmt: Int = 0, followsNc: Boolean = false) {
    require(Set("none", "mshr", "dchannel").contains(memoryService))
    val memoryForward: Boolean = memoryService != "none"
    val translated: Boolean = satp != 0 || guestPageFault
    require(!preserveTlb || guestPageFault)
    val raw: BigInt = (base + immediate) & wordMask
    val size: Int = 1 << (operation & 3)
    val effective: BigInt = {
      val keep = if (pmm == 2) 57 else if (pmm == 3) 48 else 64
      val mask = (BigInt(1) << keep) - 1
      val low = raw & mask
      (if (satp != 0 && keep != 64 && low.testBit(keep - 1)) low | (wordMask ^ mask) else low) & wordMask
    }
    val outcome: Int = {
      if (!enabled || privilege == 3 || !active) 0
      else if (virtual) 2
      else if (trusted || close) 0
      else {
        val last = effective + size - 1
        if (last <= wordMask && regions.exists(r => r.valid && r.read && r.lo < r.hi &&
          r.lo <= effective && last < r.hi)) 0 else 1
      }
    }
    val fdiCause: Option[Int] = if (outcome == 1) Some(if (privilege == 0) 24 else 25)
      else if (outcome == 2) Some(2) else None
    val standardCause: Option[Int] = if (breakpoint) Some(3)
      else if (guestPageFault) Some(21) else if (pageFault) Some(13)
      else if (pmpFault || tagError || accessFault || forwardCorrupt && outcome == 0) Some(5)
      else if (lateError) Some(19) else None
    def data: BigInt = {
      val bytes = if (forwardBytes) BigInt("0123456789abcdef0123456789abcdef", 16)
        else if (sbufferBytes) sbufferPattern
        else if (memoryService == "mshr") mshrPattern
        else if (memoryService == "dchannel") channelPattern
        else pattern
      val bits = size * 8
      val mask = (BigInt(1) << bits) - 1
      val low = (bytes >> (8 * (effective & 15).toInt)) & mask
      if (fp && size < 8) low | (wordMask ^ mask)
      else if (!fp && operation < 4 && bits < 64 && low.testBit(bits - 1)) low | (wordMask ^ mask)
      else low
    }
  }
  private case class FullAttempt(accepted: Int, id: BigInt, ordinal: Int, var killed: Boolean = false)

  private def full(dut: FDILoadPermissionHarness, events: mutable.ArrayBuffer[String], initialCycle: Int): Unit = {
    val remaining = scenario == "remaining"
    val missing = Seq.empty[Region]
    val negative = BigInt("fffffffffff08000", 16)
    val tagged7 = (BigInt(0x35) << 57) | (negative & ((BigInt(1) << 57) - 1))
    val tagged16 = (BigInt(0x2345) << 48) | (negative & ((BigInt(1) << 48) - 1))
    val basic = (0 to 6).map(op => Trial(s"integer-$op", operation = op)) ++ Seq(
      Trial("floating-word", operation = 2, fp = true), Trial("floating-double", fp = true),
      Trial("user-denied", regions = missing), Trial("supervisor-denied", privilege = 1, regions = missing),
      Trial("disabled-policy", active = false, regions = missing),
      Trial("trusted-source", trusted = true, regions = missing),
      Trial("close-read", close = true, regions = missing),
      Trial("machine-source", privilege = 3, regions = missing),
      Trial("machine-source-user-translation", privilege = 3, satp = 8, regions = missing),
      Trial("guest-enabled", virtual = true, regions = missing),
      Trial("guest-disabled", virtual = true, active = false, regions = missing),
      Trial("supervisor-guest-enabled", privilege = 1, virtual = true, regions = missing),
      Trial("supervisor-guest-disabled", privilege = 1, virtual = true, active = false, regions = missing),
      Trial("guest-close-cannot-authorize", privilege = 1, virtual = true, trusted = true, close = true),
      Trial("entry-read-denied", regions = Seq(Region(0x8000, 0x8010, read = false))),
      Trial("entry-invalid", regions = Seq(Region(0x8000, 0x8010, valid = false))),
      Trial("adjacent-not-spliced", regions = Seq(Region(0x8000, 0x8004), Region(0x8004, 0x8008))),
      Trial("empty-window", regions = Seq(Region(0x8000, 0x8000))),
      Trial("reverse-window", regions = Seq(Region(0x9000, 0x8000))),
      Trial("high-bound-anti-alias", regions = Seq(Region(BigInt("100008000", 16), BigInt("100009000", 16)))),
      Trial("exclusive-end-edge", base = 0x8008, regions = Seq(Region(0x8000, 0x8008))),
      Trial("inclusive-start-edge", base = 0x8008, regions = Seq(Region(0x8008, 0x8010))),
      Trial("gap-is-not-authorized", base = 0x8010,
        regions = Seq(Region(0x8000, 0x8008), Region(0x8018, 0x8020))),
      Trial("rv64-adder-wrap", base = wordMask - 3, immediate = 4, regions = Seq(Region(0, 8))),
      Trial("last-byte-overflow", base = wordMask - 3, regions = Seq(Region(0, wordMask)), accessFault = true),
      Trial("bare-pmm7", base = (BigInt(0x55) << 57) | 0x8000, pmm = 2),
      Trial("bare-pmm16", base = (BigInt(0xabcd) << 48) | 0x8000, pmm = 3),
      Trial("sv39-pmm16-negative", base = tagged16, pmm = 3, satp = 8, regions = Seq(Region(negative, negative + 16))),
      Trial("sv48-pmm7-negative", base = tagged7, pmm = 2, satp = 9, regions = Seq(Region(negative, negative + 16))),
      Trial("sv39-full-negative", base = negative, satp = 8, regions = Seq(Region(negative, negative + 16))),
      Trial("translated-allowed", satp = 8),
      Trial("translated-denied", satp = 8, regions = missing),
      Trial("translated-page-fault", satp = 8, pageFault = true),
      Trial("denied-page-fault", satp = 8, pageFault = true, regions = missing),
      Trial("denied-pmp-fault", pmpFault = true, regions = missing),
      Trial("denied-tag-fault", tagError = true, regions = missing),
      Trial("denied-late-hardware-error", lateError = true, regions = missing),
      Trial("allowed-late-hardware-error", lateError = true),
      Trial("fast-cache-retry", fastReplay = true),
      Trial("queue-forward-retry", normalReplay = true),
      Trial("queue-forward-retry-high-pmm", base = (BigInt(0xabcd) << 48) | 0x8000, pmm = 3, normalReplay = true),
      Trial("allowed-sq-forward", forwardBytes = true),
      Trial("denied-sq-forward", forwardBytes = true, regions = missing))
    // Cross-16-byte refusal is tested through real translation and replay. The
    // allowed split control stops at the existing L04 handoff, not invented data.
    val misaligned = Seq(false, true).flatMap { stale =>
      Seq(Trial(s"deny-miss-misaligned-hit-pmp-$stale", base = 0x800c, satp = 8,
        regions = missing, stalePmp = stale, forwardingNoise = true),
        Trial(s"deny-miss-misaligned-pf-pmp-$stale", base = 0x800c, satp = 8,
          regions = missing, stalePmp = stale, pageFault = true, forwardingNoise = true))
    }
    val controls = Seq(Trial("allow-translated-misaligned-within16", base = 0x8001, satp = 8),
      Trial("allow-translated-misaligned-pf", base = 0x8001, satp = 8, pageFault = true),
      Trial("allow-cross16-split-handoff", base = 0x800c, regions = Seq(Region(0x8000, 0x8020)), splitControl = true),
      Trial("denied-mmio-no-request", regions = missing, mmio = true),
      Trial("cancel-allowed-translation-wait", satp = 8, cancelAfterMiss = true),
      Trial("cancel-denied-translation-wait", satp = 8, regions = missing, cancelAfterMiss = true))
    val orthogonal = for (allow <- Seq(false, true); translated <- Seq(false, true); unaligned <- Seq(false, true))
      yield Trial(s"independent-allow-$allow-tlb-miss-$translated-misaligned-$unaligned",
        base = if (unaligned) 0x8001 else 0x8000, satp = if (translated) 8 else 0,
        regions = if (allow) Seq(Region(0x8000, 0x8010)) else missing)
    // Same-line allowed/denied controls keep identical bounds. Refused first
    // attempts cannot create C_DM; only a real replay may request MSHR/D data.
    val forwardRegions = Seq(Region(0x8000, 0x8008))
    val forwarding = Seq(
      Trial("sbuffer-allowed", regions = forwardRegions, sbufferBytes = true),
      Trial("sbuffer-denied", base = 0x8008, regions = forwardRegions, sbufferBytes = true),
      Trial("mshr-allowed", regions = forwardRegions, memoryService = "mshr"),
      Trial("mshr-denied-first", base = 0x8008, regions = forwardRegions, memoryService = "mshr"),
      Trial("dchannel-allowed", regions = forwardRegions, memoryService = "dchannel"),
      Trial("dchannel-denied-first", base = 0x8008, regions = forwardRegions, memoryService = "dchannel"),
      Trial("mshr-corrupt-allowed", regions = forwardRegions, memoryService = "mshr", forwardCorrupt = true),
      Trial("mshr-corrupt-denied-no-owner", base = 0x8008, regions = forwardRegions,
        memoryService = "mshr", forwardCorrupt = true))
    // The second source uses a fault entry produced by the first source's real
    // PTW transaction. Policy changes happen only after its queues have drained.
    val boundaries = Seq(
      Trial("guest-gpf-refill", base = 0x12000, rob = 16, virtual = true,
        active = false, guestPageFault = true),
      Trial("guest-illegal-gpf-hit", base = 0x12008, rob = 17, virtual = true,
        guestPageFault = true, preserveTlb = true),
      Trial("breakpoint-denied", base = 0x25010, rob = 18, breakpoint = true,
        regions = Seq(Region(0x25000, 0x25008))),
      Trial("breakpoint-allowed-control", base = 0x25000, rob = 19, breakpoint = true,
        regions = Seq(Region(0x25000, 0x25008))),
      Trial(if (enabled) "nc-denied-no-uncache" else "nc-pmp-control-no-uncache",
        base = 0x37008, rob = 20, satp = 8, pbmt = 1,
        regions = Seq(Region(0x37000, 0x37008)), pmpFault = !enabled),
      Trial("cached-progress-after-nc-refusal", base = 0x49000, rob = 21,
        regions = Seq(Region(0x49000, 0x49008)), followsNc = true))
    val cases = if (remaining) boundaries else
      basic ++ orthogonal ++ controls.filter(t => enabled || !t.mmio) ++
        (if (enabled) misaligned else Seq.empty) ++ forwarding
    require(cases.map(_.name).distinct.size == cases.size)
    var cycle = initialCycle
    var finishedCases = 0
    def log(name: String, kind: String, fields: String = ""): Unit =
      events += s"{\"cycle\":$cycle,\"case\":\"$name\",\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    def exceptionBits(value: Vec[Bool]): Set[Int] = value.zipWithIndex.collect {
      case (bit, index) if bool(bit) => index
    }.toSet
    for (trial <- cases) {
      log(trial.name, "begin")
      zero(dut.io.request.bits); dut.io.request.valid.poke(false.B)
      zero(dut.io.tlbCsr); zero(dut.io.csrCtrl); zero(dut.io.sfence)
      dut.io.trigger.foreach(zero)
      zero(dut.io.redirect.bits); dut.io.redirect.valid.poke(false.B)
      zero(dut.io.pmp); zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
      dut.io.cacheReady.poke(true.B); dut.io.cacheBankConflict.poke(false.B); dut.io.cacheNack.poke(false.B)
      dut.io.result.ready.poke(true.B)
      dut.io.ptw.req.foreach(_.ready.poke(true.B)); zero(dut.io.ptw.resp.bits)
      dut.io.ptw.resp.valid.poke(false.B); dut.io.tlbHint.valid.poke(false.B); zero(dut.io.tlbHint.bits)
      zero(dut.io.dchannel); zero(dut.io.mshrData)
      dut.io.mshrValid.poke(false.B); dut.io.mshrMatch.poke(false.B); dut.io.mshrCorrupt.poke(false.B)
      zero(dut.io.lqHead)
      for (port <- Seq(dut.io.sq, dut.io.sbuffer, dut.io.ubuffer)) {
        zero(port.forwardMaskFast); zero(port.forwardMask); zero(port.forwardData)
        port.dataInvalid.poke(false.B); port.matchInvalid.poke(false.B); port.addrInvalid.poke(false.B)
      }
      dut.io.sq.dataInvalidFast.poke(false.B); zero(dut.io.sq.dataInvalidSqIdx); zero(dut.io.sq.addrInvalidSqIdx)
      dut.io.config.foreach { config =>
        zero(config)
        config.sourcePrivilege.poke(trial.privilege.U); config.sourceVirtual.poke(trial.virtual.B)
        config.policy.uEnable.poke(trial.active.B); config.policy.sEnable.poke(trial.active.B)
        config.policy.uCloseRead.poke(trial.close.B); config.policy.sCloseRead.poke(trial.close.B)
        trial.regions.zipWithIndex.foreach { case (region, index) =>
          val entry = config.entries(index)
          entry.boundLo.poke(region.lo.U); entry.boundHi.poke(region.hi.U)
          entry.entryValid.poke(region.valid.B); entry.readAllowed.poke(region.read.B)
        }
      }
      dut.io.tlbCsr.priv.imode.poke(trial.privilege.U)
      dut.io.tlbCsr.priv.dmode.poke((if (trial.translated) 0 else 3).U)
      dut.io.tlbCsr.satp.mode.poke(trial.satp.U)
      dut.io.tlbCsr.priv.virt.poke(trial.guestPageFault.B)
      dut.io.tlbCsr.hgatp.mode.poke((if (trial.guestPageFault) 8 else 0).U)
      dut.io.tlbCsr.mPBMTE.poke((trial.pbmt != 0).B)
      dut.io.trigger.foreach { trigger =>
        if (trial.breakpoint) {
          trigger.triggerCanRaiseBpExp.poke(true.B)
          trigger.tEnableVec(0).poke(true.B)
          trigger.tdataVec(0).load.poke(true.B)
          trigger.tdataVec(0).matchType.poke(0.U)
          trigger.tdataVec(0).action.poke(0.U)
          trigger.tdataVec(0).tdata2.poke(trial.effective.U)
        }
      }
      dut.io.tlbCsr.pmm.mseccfg.poke(trial.pmm.U)
      dut.io.tlbCsr.pmm.senvcfg.poke(trial.pmm.U)
      dut.io.csrCtrl.cache_error_enable.poke(true.B)
      dut.io.csrCtrl.hd_misalign_ld_enable.poke(true.B)
      if (trial.preserveTlb || trial.followsNc) {
        if (trial.preserveTlb) {
          assert(finishedCases == 1 && cases.head.guestPageFault && !cases.head.active)
          assert((trial.effective >> 12) == (cases.head.effective >> 12))
        } else {
          assert(finishedCases == 5 && cases(finishedCases - 1).pbmt == 1)
        }
        dut.io.phases.replayAllocated.expect(0.U)
        dut.io.result.valid.expect(false.B)
        dut.io.phases.boundaries.get.uncacheAllocated.expect(0.U)
        log(trial.name, if (trial.preserveTlb) "preserve-produced-gpf-entry" else "retain-uncache-state-after-refusal")
        dut.clock.step(30); cycle += 30
      } else {
        dut.reset.poke(true.B); dut.clock.step(10); cycle += 10
        dut.reset.poke(false.B); dut.clock.step(20); cycle += 20
      }
      dut.io.request.bits.src(0).poke(trial.base.U)
      dut.io.request.bits.uop.imm.poke((trial.immediate & 0xfff).U)
      dut.io.request.bits.uop.robIdx.value.poke(trial.rob.U); dut.io.request.bits.uop.lqIdx.value.poke(0.U)
      dut.io.request.bits.uop.pdest.poke(7.U)
      dut.io.request.bits.uop.fuType.poke(FuType.ldu.U)
      dut.io.request.bits.uop.fuOpType.poke(trial.operation.U)
      dut.io.request.bits.uop.rfWen.poke((!trial.fp).B); dut.io.request.bits.uop.fpWen.poke(trial.fp.B)
      dut.io.request.bits.uop.firstUop.poke(true.B); dut.io.request.bits.uop.lastUop.poke(true.B)
      dut.io.request.bits.uop.fdiNotTrusted.foreach(_.poke((!trial.trusted).B))
      dut.io.request.bits.isFirstIssue.poke(true.B); dut.io.request.valid.poke(true.B)
      val attempts = mutable.ArrayBuffer.empty[FullAttempt]
      var accepted = false
      var completed = 0
      var split = false
      var permissionCount = 0
      var responseCount = 0
      var replayCount = 0
      var fastCount = 0
      var missCount = 0
      var waitingRows = 0
      var ptwRequests = 0
      var ptwResponses = 0
      var hintCount = 0
      var deniedStages = 0
      var pendingPermission: Option[(BigInt, Int)] = None
      var ptwDue = -1
      var refillCycle = -1
      var hintCycle = -1
      var completionCycle = -1
      var cancellationCycle = -1
      var sbufferRequestCycle = -1
      var forwardRequestCycle = -1
      var memoryWakeCycle = -1
      var memoryQueries = 0
      var effectiveForwards = 0
      var sbufferQueries = 0
      var sbufferForwards = 0
      var cacheMissRows = 0
      var actualRefills = 0
      var gpfHits = 0
      var breakpointHits = 0
      var terminalRows = 0
      var ncRows = 0
      val serviceMshrId = 1
      val deadline = cycle + 600
      val expectedVpn = (trial.raw & ((BigInt(1) << dut.VAddrBits) - 1)) >> 12
      val expectedPermissionTag = BigInt(trial.rob) <<
        (dut.io.request.bits.uop.lqIdx.value.getWidth + 1 + dut.io.request.bits.uop.uopIdx.getWidth)
      while (cycle < deadline &&
        (if (cancellationCycle >= 0) cycle < cancellationCycle + 10
         else completed == 0 && !split || completed > 0 && cycle < completionCycle + 10)) {
        dut.io.request.valid.poke((!accepted).B)
        dut.io.redirect.valid.poke(false.B)
        if (trial.cancelAfterMiss && cancellationCycle < 0 && uint(dut.io.phases.replayAllocated) > 0) {
          assert(missCount > 0 && refillCycle < 0 && completed == 0)
          dut.io.redirect.bits.robIdx.value.poke(trial.rob.U)
          dut.io.redirect.bits.level.poke(RedirectLevel.flush)
          dut.io.redirect.valid.poke(true.B)
          cancellationCycle = cycle
          ptwDue = -2; hintCycle = -2
          log(trial.name, "redirect-cancels-allocated-replay")
        }
        zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
        dut.io.cacheBankConflict.poke(false.B)
        dut.io.sq.dataInvalid.poke(false.B); dut.io.sq.dataInvalidFast.poke(false.B)
        dut.io.ptw.resp.valid.poke(false.B); dut.io.tlbHint.valid.poke(false.B)
        zero(dut.io.dchannel)
        dut.io.mshrValid.poke(false.B); dut.io.mshrMatch.poke(false.B)
        dut.io.mshrCorrupt.poke(trial.forwardCorrupt.B)
        // Data may be present without a response owner. Valid is generated only
        // from a recorded public query; non-owner noise must not select data.
        dut.io.mshrData.zipWithIndex.foreach { case (byte, index) =>
          byte.poke(((mshrPattern >> (8 * index)) & 255).U)
        }
        dut.io.sbuffer.forwardMaskFast.foreach(_.poke(false.B))
        dut.io.sbuffer.forwardMask.foreach(_.poke(false.B))
        dut.io.sbuffer.forwardData.zipWithIndex.foreach { case (byte, index) =>
          byte.poke(((sbufferPattern >> (8 * index)) & 255).U)
        }
        if (trial.sbufferBytes && bool(dut.io.sbuffer.valid)) {
          dut.io.sbuffer.uop.robIdx.value.expect(trial.rob.U)
          dut.io.sbuffer.vaddr.expect((trial.effective & ((BigInt(1) << dut.VAddrBits) - 1)).U)
          dut.io.sbuffer.paddr.expect(trial.effective.U)
          val expectedMask = ((BigInt(1) << trial.size) - 1) << (trial.effective & 15).toInt
          dut.io.sbuffer.mask.expect(expectedMask.U)
          sbufferRequestCycle = cycle; sbufferQueries += 1
          dut.io.sbuffer.forwardMaskFast.foreach(_.poke(true.B))
          log(trial.name, "sbuffer-query", s"\"paddr\":${trial.effective},\"mask\":$expectedMask")
        }
        if (trial.sbufferBytes && cycle == sbufferRequestCycle + 1 && sbufferRequestCycle >= 0) {
          dut.io.sbuffer.forwardMask.foreach(_.poke(true.B))
          sbufferForwards += 1
        }
        if (trial.memoryForward) {
          // The service notification wakes a real allocated C_DM entry. It is
          // not a manufactured replay request or a model of grant ownership.
          if (memoryWakeCycle < 0 && uint(dut.io.phases.replayAllocated) > 0) memoryWakeCycle = cycle
          val query = bool(dut.io.phases.forwardRequest.valid)
          val noise = bool(dut.io.phases.s1) && !query
          val channelBeat = memoryWakeCycle == cycle || noise || query && trial.memoryService == "dchannel"
          dut.io.dchannel.valid.poke(channelBeat.B)
          dut.io.dchannel.mshrid.poke(serviceMshrId.U)
          dut.io.dchannel.last.poke(trial.effective.testBit(5).B)
          dut.io.dchannel.data.poke(channelPattern.U)
          if (memoryWakeCycle == cycle) log(trial.name, "cache-miss-notification", s"\"mshr\":$serviceMshrId")
          if (noise) log(trial.name, "unowned-forward-data", s"\"mshr\":$serviceMshrId")
          if (query) {
            dut.io.phases.forwardRequest.bits.mshrid.expect(serviceMshrId.U)
            dut.io.phases.forwardRequest.bits.paddr.expect(trial.effective.U)
            assert(replayCount > 0 && trial.outcome == 0, "Forward query did not belong to an allowed real C_DM replay")
            forwardRequestCycle = cycle; memoryQueries += 1
            log(trial.name, "memory-forward-query", s"\"mshr\":$serviceMshrId,\"paddr\":${trial.effective}")
          }
          if (forwardRequestCycle >= 0 && cycle == forwardRequestCycle + 1) {
            dut.io.mshrValid.poke(true.B)
            dut.io.mshrMatch.poke((trial.memoryService == "mshr").B)
            val mshrReply = if (trial.effective.testBit(3)) {
              val word = (mshrPattern >> 64) & wordMask
              word | (word << 64)
            } else mshrPattern
            dut.io.mshrData.zipWithIndex.foreach { case (byte, index) =>
              byte.poke(((mshrReply >> (8 * index)) & 255).U)
            }
            effectiveForwards += 1
            dut.io.phases.s2.expect(true.B)
            dut.io.phases.s2Rob.expect(2.U)
            dut.io.phases.s2ForwardResult.expect(true.B)
            dut.io.phases.s2ForwardD.expect((trial.memoryService == "dchannel").B)
            dut.io.phases.s2ForwardMshr.expect((trial.memoryService == "mshr").B)
            log(trial.name, "memory-forward-response", s"\"source\":\"${trial.memoryService}\",\"corrupt\":${trial.forwardCorrupt}")
          } else {
            dut.io.phases.s2ForwardResult.expect(false.B)
            dut.io.phases.s2ForwardMshr.expect(false.B)
          }
        }
        val stale = trial.translated && refillCycle < 0 && !trial.preserveTlb
        dut.io.pmp.ld.poke((trial.pmpFault || trial.stalePmp && stale).B)
        dut.io.pmp.mmio.poke(trial.mmio.B)
        dut.io.sq.matchInvalid.poke((trial.forwardingNoise && stale).B)
        val s1Attempt = attempts.find(_.accepted + 1 == cycle)
        dut.io.sq.dataInvalidFast.poke((trial.normalReplay && s1Attempt.exists(_.ordinal == 0)).B)
        s1Attempt.foreach(item => item.killed = bool(dut.io.cacheS1Kill))
        val s2Attempt = attempts.find(_.accepted + 2 == cycle)
        s2Attempt.filterNot(_.killed).foreach { item =>
          dut.io.cacheResponse.valid.poke(true.B)
          dut.io.cacheResponse.bits.id.poke(item.id.U)
          dut.io.cacheResponse.bits.data.poke(pattern.U)
          dut.io.cacheResponse.bits.tag_error.poke(trial.tagError.B)
          if (trial.memoryForward) {
            dut.io.cacheResponse.bits.miss.poke(true.B)
            dut.io.cacheResponse.bits.handled.poke(true.B)
            dut.io.cacheResponse.bits.mshr_id.poke(serviceMshrId.U)
          }
          dut.io.cacheBankConflict.poke((trial.fastReplay && item.ordinal == 0).B)
          dut.io.sq.dataInvalid.poke((trial.normalReplay && item.ordinal == 0).B)
        }
        attempts.find(item => item.accepted + 3 == cycle && !item.killed).foreach { _ =>
          dut.io.cacheResponse.bits.data_delayed.poke(pattern.U)
          dut.io.cacheResponse.bits.error_delayed.poke(trial.lateError.B)
        }
        dut.io.sq.forwardMaskFast.foreach(_.poke(trial.forwardBytes.B))
        dut.io.sq.forwardMask.foreach(_.poke((trial.forwardBytes && s2Attempt.exists(!_.killed)).B))
        dut.io.sq.forwardData.zipWithIndex.foreach { case (byte, index) =>
          byte.poke(((BigInt("0123456789abcdef0123456789abcdef", 16) >> (8 * index)) & 255).U)
        }
        if (ptwDue == cycle) {
          val vpn = expectedVpn
          val sector = dut.io.ptw.resp.bits.s1.pteidx.length
          val index = (vpn % sector).toInt
          zero(dut.io.ptw.resp.bits)
          val response = dut.io.ptw.resp.bits
          response.s2xlate.poke((if (trial.guestPageFault) 2 else 0).U)
          response.memidx.is_ld.poke(true.B); response.memidx.idx.poke(0.U)
          response.s1.entry.tag.poke((vpn / sector).U)
          response.s1.entry.level.foreach(_.poke(0.U))
          response.s1.entry.ppn.poke((BigInt(0x90) / sector).U)
          response.s1.entry.v.poke((!trial.pageFault).B)
          response.s1.entry.pbmt.poke(trial.pbmt.U)
          response.s1.entry.perm.foreach { perm =>
            perm.r.poke(true.B); perm.a.poke(true.B); perm.d.poke(true.B); perm.u.poke(true.B)
          }
          response.s1.addr_low.poke(index.U)
          response.s1.pf.poke(trial.pageFault.B)
          response.s1.valididx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
          response.s1.pteidx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
          response.s1.ppn_low.zipWithIndex.foreach { case (low, i) => low.poke(i.U) }
          if (trial.guestPageFault) {
            response.s2.entry.tag.poke(vpn.U)
            response.s2.entry.level.foreach(_.poke(0.U))
            response.s2.entry.ppn.poke(0xb0.U)
            response.s2.entry.v.poke(false.B)
            response.s2.entry.perm.foreach { perm =>
              perm.r.poke(true.B); perm.a.poke(true.B); perm.d.poke(true.B); perm.u.poke(true.B)
            }
            response.s2.gpf.poke(true.B)
          }
          assert(ptwRequests > 0 && !trial.preserveTlb, "Refill lacks a real outstanding PTW request")
          dut.io.ptw.resp.valid.poke(true.B)
          assert(bool(dut.io.ptw.resp.ready), "PTW service response was not accepted")
          refillCycle = cycle; hintCycle = cycle + 4
          ptwResponses += 1
          log(trial.name, "ptw-refill", s"\"vpn\":$vpn,\"page_fault\":${trial.pageFault},\"guest_page_fault\":${trial.guestPageFault},\"pbmt\":${trial.pbmt}")
        }
        if (cycle == hintCycle) {
          dut.io.tlbHint.valid.poke(true.B); dut.io.tlbHint.bits.replay_all.poke(true.B)
          hintCount += 1
          log(trial.name, "tlb-hint-after-refill")
        }
        if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) {
          assert(!accepted); accepted = true
          log(trial.name, "issue", s"\"raw\":${trial.raw},\"effective\":${trial.effective}")
        }
        if (bool(dut.io.cacheRequest.valid)) {
          assert(uint(dut.io.phases.s0Rob) == trial.rob)
          attempts += FullAttempt(cycle, uint(dut.io.cacheRequest.bits.id), attempts.size)
          log(trial.name, "cache-attempt", s"\"attempt\":${attempts.size}")
        }
        for ((request, lane) <- dut.io.ptw.req.zipWithIndex if bool(request.valid) && bool(request.ready)) {
          assert(trial.translated && lane == 0 && !trial.preserveTlb, "Unexpected PTW lane or translation context")
          request.bits.vpn.expect(expectedVpn.U)
          request.bits.s2xlate.expect((if (trial.guestPageFault) 2 else 0).U)
          request.bits.getGpa.expect(false.B)
          request.bits.memidx.is_ld.expect(true.B)
          request.bits.memidx.idx.expect(0.U)
          ptwRequests += 1
          if (ptwDue == -1) ptwDue = cycle + 12
          log(trial.name, "ptw-request", s"\"vpn\":$expectedVpn,\"lq\":0,\"lane\":$lane")
        }
        if (remaining) {
          val boundary = dut.io.phases.boundaries.get
          if (bool(boundary.tlbRefill)) {
            assert(cycle == refillCycle && ptwResponses == 1)
            actualRefills += 1
            log(trial.name, "actual-tlb-refill")
          }
          if (bool(dut.io.phases.tlbResponse.valid) && !bool(dut.io.phases.tlbResponse.bits.miss)) {
            val translated = dut.io.phases.tlbResponse.bits
            if (trial.guestPageFault) {
              translated.excp(0).gpf.ld.expect(true.B)
              translated.excp(0).pf.ld.expect(false.B)
              translated.fullva.expect(trial.effective.U)
              translated.gpaddr(0).expect(trial.effective.U)
              assert(trial.preserveTlb || refillCycle >= 0 && cycle > refillCycle)
              gpfHits += 1
              log(trial.name, "actual-gpf-tlb-hit", s"\"rob\":${trial.rob},\"fullva\":${trial.effective},\"gpa\":${uint(translated.gpaddr(0))}")
            }
            if (trial.pbmt != 0) {
              translated.pbmt(0).expect(trial.pbmt.U)
              log(trial.name, "actual-pbmt-tlb-hit", s"\"pbmt\":${trial.pbmt}")
            }
          }
          if (bool(dut.io.phases.s1) && trial.breakpoint) {
            boundary.s1Trigger.expect(0.U)
            breakpointHits += 1
            log(trial.name, "actual-memtrigger-breakpoint", s"\"rob\":${trial.rob},\"address\":${trial.effective}")
          }
          if (bool(dut.io.phases.s2) && !bool(dut.io.phases.s2TlbMiss)) {
            dut.io.phases.s2Rob.expect(trial.rob.U)
            if (trial.guestPageFault) boundary.s2Exceptions(21).expect(true.B)
            if (trial.breakpoint) boundary.s2Exceptions(3).expect(true.B)
          }
          boundary.uncacheInput.expect((bool(dut.io.phases.queueWrite.valid) &&
            !bool(dut.io.phases.queueWrite.bits.nc_with_data)).B)
          boundary.uncacheEnqueue.expect(false.B)
          boundary.uncacheAllocated.expect(0.U)
          boundary.uncacheRequest.expect(false.B)
          boundary.uncacheReturn.expect(false.B)
          boundary.uncacheRollback.expect(false.B)
          if (bool(dut.io.phases.queueWrite.valid) && !bool(dut.io.phases.queueWrite.bits.tlbMiss)) {
            val row = dut.io.phases.queueWrite.bits
            row.uop.robIdx.value.expect(trial.rob.U)
            val required = trial.fdiCause.toSet ++ trial.standardCause.toSet
            assert(exceptionBits(row.uop.exceptionVec) == required)
            assert(required.nonEmpty || trial.followsNc)
            terminalRows += 1
            if (trial.pbmt != 0) {
              row.nc.expect(true.B)
              row.mmio.expect(false.B)
              row.nc_with_data.expect(false.B)
              ncRows += 1
            }
            log(trial.name, if (required.nonEmpty) "uncache-terminal-row-suppressed" else "cached-following-row",
              s"\"rob\":${trial.rob},\"nc\":${bool(row.nc)},\"faults\":[${required.toSeq.sorted.mkString(",")}]")
          }
        }
        // Check the previous accepted request before accepting a possible replacement.
        if (bool(dut.io.phases.permissionResponse.valid)) {
          responseCount += 1
          assert(pendingPermission.nonEmpty, "Unowned permission response")
          val (tag, acceptedAt) = pendingPermission.get
          assert(cycle > acceptedAt)
          val response = dut.io.phases.permissionResponse.bits
          response.outcome.expect(trial.outcome.U)
          response.request.tag.expect(tag.U)
          response.request.address.expect(trial.effective.U)
          response.request.sizeLog2.expect((trial.operation & 3).U)
          response.request.sourcePrivilege.expect(trial.privilege.U)
          response.request.sourceVirtual.expect(trial.virtual.B)
          response.request.notTrusted.expect((!trial.trusted).B)
          dut.io.phases.permissionConsumed.expect(true.B)
          pendingPermission = None
        }
        if (bool(dut.io.phases.permissionRequest.valid)) {
          permissionCount += 1
          val request = dut.io.phases.permissionRequest.bits
          request.address.expect(trial.effective.U); request.sourcePrivilege.expect(trial.privilege.U)
          request.sourceVirtual.expect(trial.virtual.B); request.notTrusted.expect((!trial.trusted).B)
          request.sizeLog2.expect((trial.operation & 3).U); request.operation.expect(0.U); request.pc.expect(0.U)
          request.tag.expect(expectedPermissionTag.U)
          assert(pendingPermission.isEmpty)
          pendingPermission = Some((expectedPermissionTag, cycle))
        }
        if (bool(dut.io.phases.s2)) {
          if (trial.sbufferBytes) dut.io.phases.s2FullForward.expect(true.B)
          if (trial.memoryForward && trial.outcome != 0) {
            dut.io.phases.forwardRequest.valid.expect(false.B)
            dut.io.phases.s2ForwardD.expect(false.B)
            dut.io.phases.s2ForwardMshr.expect(false.B)
            dut.io.phases.s2ForwardResult.expect(false.B)
          }
          if (enabled) dut.io.phases.s2Address.expect(trial.effective.U)
          if (bool(dut.io.phases.s2TlbMiss)) {
            missCount += 1
            if (trial.outcome == 1) {
              dut.io.phases.translationRetry.expect(true.B)
              dut.io.phases.blocked.expect(true.B)
              dut.io.phases.fast.expect(false.B); dut.io.phases.split.expect(false.B)
            }
          }
          if (trial.outcome != 0) {
            dut.io.phases.ld1Cancel.expect(true.B)
            dut.io.phases.fast.expect(false.B)
            dut.io.cacheS2Kill.expect(true.B)
          }
        }
        dut.io.phases.l2l.expect(false.B)
        if (bool(dut.io.phases.s3) && trial.outcome != 0) {
          deniedStages += 1
          dut.io.phases.ld2Cancel.expect(true.B)
          dut.io.phases.split.expect(false.B)
        }
        if (bool(dut.io.phases.queueWrite.valid) && bool(dut.io.phases.queueWrite.bits.tlbMiss) && trial.outcome == 1) {
          waitingRows += 1
          assert(exceptionBits(dut.io.phases.queueWrite.bits.uop.exceptionVec).isEmpty,
            "Unresolved denied translation became a terminal address or FDI fault")
          val causes = exceptionBits(dut.io.phases.queueWrite.bits.rep_info.cause)
          assert(causes == Set(1), s"Unresolved denied translation lost its sole TLB replay: $causes")
          dut.io.phases.rollback.expect(false.B)
          dut.io.result.valid.expect(false.B)
        }
        if (bool(dut.io.phases.queueWrite.valid) && trial.outcome != 0)
          dut.io.phases.queueWrite.bits.mmio.expect(false.B)
        if (trial.memoryForward && bool(dut.io.phases.queueWrite.valid)) {
          val row = dut.io.phases.queueWrite.bits
          row.uop.robIdx.value.expect(trial.rob.U)
          if (bool(row.rep_info.cause(4))) {
            assert(trial.outcome == 0, "A refused first attempt created C_DM")
            assert(exceptionBits(row.rep_info.cause) == Set(4))
            row.handledByMSHR.expect(true.B)
            row.rep_info.mshr_id.expect(serviceMshrId.U)
            cacheMissRows += 1
            log(trial.name, "cache-miss-replay-row", s"\"mshr\":$serviceMshrId,\"rob\":${trial.rob}")
          }
        }
        if (bool(dut.io.phases.replay.valid)) {
          assert(cancellationCycle < 0, "Cancelled replay entry was selected again")
          replayCount += 1
          val item = dut.io.phases.replay.bits
          if (enabled) item.fullva.expect(trial.effective.U)
          item.uop.robIdx.value.expect(trial.rob.U)
          item.uop.lqIdx.value.expect(0.U)
          item.isFirstIssue.expect(false.B)
          item.uop.fdiNotTrusted.foreach(_.expect((!trial.trusted).B))
          item.fdiSourcePrivilege.foreach(_.expect(trial.privilege.U))
          item.fdiSourceVirtual.foreach(_.expect(trial.virtual.B))
          if (trial.memoryForward) {
            item.forward_tlDchannel.expect(true.B)
            item.mshrid.expect(serviceMshrId.U)
          }
          log(trial.name, "queue-replay", s"\"fullva\":${uint(item.fullva)}")
        }
        if (bool(dut.io.phases.fastReplay.valid)) {
          fastCount += 1
          if (enabled) dut.io.phases.fastReplay.bits.fullva.expect(trial.effective.U)
        }
        if (bool(dut.io.phases.split)) {
          assert(trial.splitControl && trial.outcome == 0, "Refused or unresolved load entered misalignment splitting")
          assert(completed == 0)
          split = true; completionCycle = cycle
          log(trial.name, "allowed-split-handoff")
        }
        if (bool(dut.io.result.valid) && bool(dut.io.result.ready)) {
          assert(cancellationCycle < 0, "Cancelled translation completed a stale instruction")
          completed += 1; assert(completed == 1, "Duplicate terminal completion")
          completionCycle = cycle
          val faults = exceptionBits(dut.io.result.bits.uop.exceptionVec)
          val required = trial.fdiCause.toSet ++ trial.standardCause.toSet
          assert(faults == required, s"${trial.name}: expected faults $required, observed $faults")
          if (trial.outcome != 0) {
            dut.io.result.bits.uop.rfWen.expect(false.B); dut.io.result.bits.uop.fpWen.expect(false.B)
            dut.io.result.bits.data.expect(0.U)
            dut.io.phases.ld2Cancel.expect(true.B)
            dut.io.phases.s3.expect(true.B)
            dut.io.phases.l2l.expect(false.B)
            if (trial.outcome == 1) {
              dut.io.result.bits.uop.fdiException.get.tval.expect(trial.effective.U)
              dut.io.result.bits.uop.fdiException.get.reason.expect(2.U)
            }
          } else if (trial.standardCause.isEmpty) {
            dut.io.result.bits.data.expect(trial.data.U)
            dut.io.result.bits.uop.rfWen.expect((!trial.fp).B); dut.io.result.bits.uop.fpWen.expect(trial.fp.B)
          }
          log(trial.name, "completion", s"\"faults\":[${faults.toSeq.sorted.mkString(",")}],\"data\":${uint(dut.io.result.bits.data)}")
        }
        dut.clock.step(); cycle += 1
      }
      assert(accepted && (completed == 1 || split || cancellationCycle >= 0),
        s"${trial.name}: no terminal result, allowed handoff or proven cancellation")
      assert(permissionCount == (if (enabled) attempts.size else 0) && responseCount == permissionCount)
      assert(pendingPermission.isEmpty)
      if (trial.outcome != 0) assert(deniedStages > 0)
      if (!split) dut.io.phases.replayAllocated.expect(0.U)
      if (trial.cancelAfterMiss) {
        assert(cancellationCycle >= 0 && missCount > 0 && ptwRequests > 0 && completed == 0 && ptwResponses == 0)
        dut.io.phases.replayAllocated.expect(0.U)
        dut.io.phases.replay.valid.expect(false.B)
        dut.io.result.valid.expect(false.B)
      } else if (trial.translated && !trial.preserveTlb && !trial.splitControl && trial.outcome != 2) {
        assert(missCount > 0 && refillCycle >= 0 && replayCount > 0,
          s"${trial.name}: did not traverse real TLB miss/refill/replay")
        assert(ptwRequests > 0 && ptwResponses == 1 && hintCount == 1)
        assert(completionCycle > refillCycle)
        if (trial.outcome == 1) assert(waitingRows > 0)
      }
      if (remaining) {
        assert(terminalRows == 1)
        assert(actualRefills == (if (trial.translated && !trial.preserveTlb) 1 else 0))
        assert(gpfHits == (if (trial.guestPageFault) 1 else 0))
        assert(breakpointHits == (if (trial.breakpoint) 1 else 0))
        assert(ncRows == (if (trial.pbmt != 0) 1 else 0))
        if (trial.preserveTlb) {
          assert(ptwRequests == 0 && ptwResponses == 0 && missCount == 0 && replayCount == 0 && attempts.size == 1)
          assert(permissionCount == (if (enabled) 1 else 0))
        }
        log(trial.name, "boundary-drained", s"\"terminal_rows\":$terminalRows,\"refills\":$actualRefills,\"gpf_hits\":$gpfHits," +
          s"\"breakpoint_hits\":$breakpointHits,\"nc_rows\":$ncRows,\"uncache_allocations\":0,\"uncache_requests\":0")
      }
      if (trial.fastReplay) assert(fastCount > 0 && attempts.size >= 2)
      if (trial.normalReplay) assert(replayCount > 0 && attempts.size >= 2)
      if (trial.sbufferBytes) assert(sbufferQueries == 1 && sbufferForwards == 1)
      if (trial.memoryForward && trial.outcome == 0) {
        assert(memoryQueries == 1 && effectiveForwards == 1 && replayCount == 1 && attempts.size == 2)
        assert(cacheMissRows == 1)
        assert(memoryWakeCycle >= 0 && memoryWakeCycle < forwardRequestCycle)
      } else if (trial.memoryForward) {
        assert(memoryQueries == 0 && effectiveForwards == 0 && replayCount == 0 && attempts.size == 1)
        assert(cacheMissRows == 0)
        assert(memoryWakeCycle < 0, "Denied first attempt allocated a cache-miss replay")
      }
      finishedCases += 1
      log(trial.name, "complete", s"\"attempts\":${attempts.size},\"tlb_misses\":$missCount,\"queue_replays\":$replayCount,\"fast_replays\":$fastCount")
    }
    if (remaining) {
      println(s"L01_LOCAL_REMAINING_PASS enabled=$enabled cases=$finishedCases cycles=$cycle actual_tlb=true actual_uncache=true actual_memtrigger=true")
    } else {
      cycle = cancellation(dut, events, cycle)
      cycle = fastReplayOverlap(dut, events, cycle)
      println(s"L01_LOCAL_FULL_PASS enabled=$enabled cases=$finishedCases cancellationCases=4 overlapCases=1 cycles=$cycle actual_tlb=true actual_replay=true l2l_enabled=false")
    }
  }

  private def cancellation(dut: FDILoadPermissionHarness, events: mutable.ArrayBuffer[String], initialCycle: Int): Int = {
    var cycle = initialCycle
    for (kind <- Seq("s1-cancel", "s2-older-replacement", "s2-wrap-replacement", "reset-held-permission")) {
      val wrap = kind == "s2-wrap-replacement"
      val victim = Source(if (wrap) 0 else 3, 0x8010, BigInt("bad0bad1bad2bad3", 16), flag = wrap)
      val survivor = Source(if (wrap) dut.RobSize - 1 else 2, 0x8000, BigInt("1020304050607080", 16))
      val sources = Seq(victim, survivor)
      def permissionTag(source: Source): BigInt = {
        val rob = BigInt(source.rob) | (if (source.flag) BigInt(1) << dut.io.request.bits.uop.robIdx.value.getWidth else BigInt(0))
        val lq = if (source eq victim) BigInt(1) else BigInt(0)
        val uopWidth = dut.io.request.bits.uop.uopIdx.getWidth
        (rob << (dut.io.request.bits.uop.lqIdx.value.getWidth + 1 + uopWidth)) | (lq << uopWidth)
      }
      val issued = mutable.Map.empty[Int, Int]
      val attempts = mutable.ArrayBuffer.empty[CacheAttempt]
      val s1Killed = mutable.Set.empty[Int]
      val completed = mutable.Set.empty[Int]
      var redirected = false
      var resetApplied = false
      var resetEnded = false
      var replacement = false
      def log(event: String, detail: String = ""): Unit =
        events += s"{\"cycle\":$cycle,\"case\":\"$kind\",\"event\":\"$event\"${if (detail.isEmpty) "" else "," + detail}}"
      zero(dut.io.request.bits); dut.io.request.valid.poke(false.B)
      zero(dut.io.tlbCsr); zero(dut.io.csrCtrl); zero(dut.io.sfence)
      dut.io.trigger.foreach(zero)
      zero(dut.io.redirect.bits); dut.io.redirect.valid.poke(false.B)
      zero(dut.io.pmp); zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
      dut.io.cacheReady.poke(true.B); dut.io.cacheBankConflict.poke(false.B); dut.io.cacheNack.poke(false.B)
      dut.io.result.ready.poke(true.B)
      dut.io.ptw.req.foreach(_.ready.poke(true.B)); zero(dut.io.ptw.resp.bits); dut.io.ptw.resp.valid.poke(false.B)
      dut.io.tlbHint.valid.poke(false.B); zero(dut.io.tlbHint.bits)
      zero(dut.io.dchannel); zero(dut.io.mshrData)
      dut.io.mshrValid.poke(false.B); dut.io.mshrMatch.poke(false.B); dut.io.mshrCorrupt.poke(false.B)
      zero(dut.io.lqHead)
      for (port <- Seq(dut.io.sq, dut.io.sbuffer, dut.io.ubuffer)) {
        zero(port.forwardMaskFast); zero(port.forwardMask); zero(port.forwardData)
        port.dataInvalid.poke(false.B); port.matchInvalid.poke(false.B); port.addrInvalid.poke(false.B)
      }
      dut.io.sq.dataInvalidFast.poke(false.B); zero(dut.io.sq.dataInvalidSqIdx); zero(dut.io.sq.addrInvalidSqIdx)
      dut.io.config.foreach { config =>
        zero(config); config.sourcePrivilege.poke(0.U); config.policy.uEnable.poke(true.B)
        config.entries(0).entryValid.poke(true.B); config.entries(0).readAllowed.poke(true.B)
        config.entries(0).boundLo.poke(0x8000.U); config.entries(0).boundHi.poke(0x8008.U)
      }
      dut.io.tlbCsr.priv.imode.poke(0.U); dut.io.tlbCsr.priv.dmode.poke(3.U)
      dut.io.csrCtrl.cache_error_enable.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(10); cycle += 10
      dut.reset.poke(false.B); dut.clock.step(20); cycle += 20
      val start = cycle
      val resetCase = kind == "reset-held-permission"
      var tail = -1
      while (cycle < start + 100 && (tail < 0 || cycle < tail + 10)) {
        val offer = if (!issued.contains(victim.rob)) Some(victim)
          else if (!issued.contains(survivor.rob) && (!resetCase || resetEnded)) Some(survivor) else None
        dut.io.request.valid.poke(offer.nonEmpty.B)
        offer.foreach { source =>
          zero(dut.io.request.bits)
          dut.io.request.bits.src(0).poke(source.address.U)
          dut.io.request.bits.uop.robIdx.value.poke(source.rob.U)
          dut.io.request.bits.uop.robIdx.flag.poke(source.flag.B)
          dut.io.request.bits.uop.lqIdx.value.poke((if (source eq victim) 1 else 0).U)
          dut.io.request.bits.uop.pdest.poke(7.U)
          dut.io.request.bits.uop.fuType.poke(FuType.ldu.U); dut.io.request.bits.uop.fuOpType.poke(3.U)
          dut.io.request.bits.uop.rfWen.poke(true.B)
          dut.io.request.bits.uop.firstUop.poke(true.B); dut.io.request.bits.uop.lastUop.poke(true.B)
          dut.io.request.bits.uop.fdiNotTrusted.foreach(_.poke(true.B))
          dut.io.request.bits.isFirstIssue.poke(true.B)
        }
        val act = issued.get(victim.rob).exists(_ + (if (kind == "s1-cancel") 1 else 2) == cycle)
        dut.io.redirect.valid.poke((act && !resetCase).B)
        if (act && !resetCase) {
          dut.io.redirect.bits.robIdx.value.poke(survivor.rob.U)
          dut.io.redirect.bits.robIdx.flag.poke(survivor.flag.B)
          dut.io.redirect.bits.level.poke(RedirectLevel.flushAfter)
          redirected = true
          log("selective-redirect", s"\"victim_rob\":${victim.rob},\"victim_flag\":${victim.flag}," +
            s"\"survivor_rob\":${survivor.rob},\"survivor_flag\":${survivor.flag}")
        }
        zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
        attempts.find(_.accepted + 1 == cycle).foreach { item =>
          if (bool(dut.io.cacheS1Kill)) s1Killed += item.accepted
        }
        attempts.find(item => item.accepted + 2 == cycle && !s1Killed(item.accepted)).foreach { item =>
          dut.io.cacheResponse.valid.poke(true.B); dut.io.cacheResponse.bits.id.poke(item.id.U)
          dut.io.cacheResponse.bits.data.poke((item.source.data | (item.source.data << 64)).U)
        }
        attempts.find(item => item.accepted + 3 == cycle && !s1Killed(item.accepted)).foreach { item =>
          dut.io.cacheResponse.bits.data_delayed.poke((item.source.data | (item.source.data << 64)).U)
        }
        if (act && resetCase) {
          if (enabled) {
            dut.io.phases.permissionResponse.valid.expect(true.B)
            dut.io.phases.permissionResponse.bits.request.address.expect(victim.address.U)
            dut.io.phases.permissionResponse.bits.request.tag.expect(permissionTag(victim).U)
          }
          dut.io.request.valid.poke(false.B)
          dut.reset.poke(true.B)
          resetApplied = true
          log("reset-with-owned-permission")
        }
        if (!act || !resetCase) {
          if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) {
            val source = offer.get
            assert(!issued.contains(source.rob)); issued(source.rob) = cycle
            log("issue", s"\"rob\":${source.rob},\"flag\":${source.flag},\"address\":${source.address}")
          }
          if (bool(dut.io.cacheRequest.valid)) {
            val source = offer.getOrElse(throw new AssertionError("Cancellation case unexpectedly replayed"))
            attempts += CacheAttempt(source, cycle, uint(dut.io.cacheRequest.bits.id))
          }
          if (enabled && act && kind.contains("replacement")) {
            dut.io.phases.permissionResponse.valid.expect(true.B)
            dut.io.phases.permissionResponse.bits.request.address.expect(victim.address.U)
            dut.io.phases.permissionResponse.bits.request.tag.expect(permissionTag(victim).U)
            dut.io.phases.permissionConsumed.expect(true.B)
            dut.io.phases.permissionRequest.valid.expect(true.B)
            dut.io.phases.permissionRequest.bits.address.expect(survivor.address.U)
            dut.io.phases.permissionRequest.bits.tag.expect(permissionTag(survivor).U)
            replacement = true
            log("discard-victim-and-accept-older-permission")
          }
          if (bool(dut.io.phases.permissionRequest.valid)) {
            val address = uint(dut.io.phases.permissionRequest.bits.address)
            val source = sources.find(_.address == address).get
            dut.io.phases.permissionRequest.bits.tag.expect(permissionTag(source).U)
          }
          if (bool(dut.io.result.valid) && bool(dut.io.result.ready)) {
            dut.io.result.bits.uop.robIdx.value.expect(survivor.rob.U)
            dut.io.result.bits.uop.robIdx.flag.expect(survivor.flag.B)
            assert(completed.add(survivor.rob), "Surviving load completed more than once")
            dut.io.result.bits.uop.exceptionVec.foreach(_.expect(false.B))
            dut.io.result.bits.data.expect(survivor.data.U)
            dut.io.result.bits.uop.rfWen.expect(true.B)
            tail = cycle
            log("surviving-allowed-completion")
          }
          dut.io.phases.l2l.expect(false.B)
          dut.io.phases.replay.valid.expect(false.B); dut.io.phases.split.expect(false.B)
        }
        dut.clock.step(); cycle += 1
        if (act && resetCase) {
          dut.clock.step(4); cycle += 4
          dut.reset.poke(false.B)
          resetEnded = true
          dut.io.phases.permissionResponse.valid.expect(false.B)
          dut.io.result.valid.expect(false.B)
        }
      }
      assert(completed == Set(survivor.rob) && issued.size == 2)
      assert(if (resetCase) resetApplied else redirected)
      if (enabled && kind.contains("replacement")) assert(replacement)
      dut.io.phases.replayAllocated.expect(0.U)
      dut.io.phases.permissionResponse.valid.expect(false.B)
      dut.io.result.valid.expect(false.B)
      log("complete")
    }
    cycle
  }

  // A normal younger source translates immediately before an older fast retry.
  // The retry must use its saved address while the TLB response payload holds
  // the other source and that source is still live in the neighboring stage.
  private def fastReplayOverlap(dut: FDILoadPermissionHarness,
    events: mutable.ArrayBuffer[String], initialCycle: Int): Int = {
    val name = "fast-noquery-overlaps-other-tlb-owner"
    val first = Source(12, 0x8000, BigInt("19a2b3c4d5e6f708", 16))
    val other = Source(13, 0x8008, BigInt("e8d7c6b5a4938271", 16))
    val byRob = Seq(first, other).map(x => x.rob -> x).toMap
    val attempts = mutable.ArrayBuffer.empty[CacheAttempt]
    val issued = mutable.Map.empty[Int, Int]
    val completions = mutable.Set.empty[Int]
    val permissionRequests = mutable.Map.empty[Int, Int].withDefaultValue(0)
    val permissionResponses = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var pendingPermission: Option[Source] = None
    var cycle = initialCycle
    var fastReplays = 0
    var overlap = false
    var otherTlbResponse = false
    var tail = -1
    def record(kind: String, fields: String = ""): Unit =
      events += s"{\"cycle\":$cycle,\"case\":\"$name\",\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    def lq(source: Source): Int = if (source eq first) 0 else 1
    def permissionTag(source: Source): BigInt =
      (BigInt(source.rob) << (dut.io.request.bits.uop.lqIdx.value.getWidth + 1 + dut.io.request.bits.uop.uopIdx.getWidth)) |
        (BigInt(lq(source)) << dut.io.request.bits.uop.uopIdx.getWidth)

    zero(dut.io.request.bits); dut.io.request.valid.poke(false.B)
    zero(dut.io.tlbCsr); zero(dut.io.csrCtrl); zero(dut.io.sfence)
    zero(dut.io.redirect.bits); dut.io.redirect.valid.poke(false.B)
    zero(dut.io.pmp); zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
    dut.io.cacheReady.poke(true.B); dut.io.cacheBankConflict.poke(false.B); dut.io.cacheNack.poke(false.B)
    dut.io.result.ready.poke(true.B)
    dut.io.ptw.req.foreach(_.ready.poke(true.B)); zero(dut.io.ptw.resp.bits); dut.io.ptw.resp.valid.poke(false.B)
    zero(dut.io.tlbHint.bits); dut.io.tlbHint.valid.poke(false.B)
    zero(dut.io.dchannel); zero(dut.io.mshrData)
    dut.io.mshrValid.poke(false.B); dut.io.mshrMatch.poke(false.B); dut.io.mshrCorrupt.poke(false.B)
    zero(dut.io.lqHead)
    for (port <- Seq(dut.io.sq, dut.io.sbuffer, dut.io.ubuffer)) {
      zero(port.forwardMaskFast); zero(port.forwardMask); zero(port.forwardData)
      port.dataInvalid.poke(false.B); port.matchInvalid.poke(false.B); port.addrInvalid.poke(false.B)
    }
    dut.io.sq.dataInvalidFast.poke(false.B); zero(dut.io.sq.dataInvalidSqIdx); zero(dut.io.sq.addrInvalidSqIdx)
    dut.io.config.foreach { config =>
      zero(config); config.sourcePrivilege.poke(0.U); config.policy.uEnable.poke(true.B)
      config.entries(0).entryValid.poke(true.B); config.entries(0).readAllowed.poke(true.B)
      config.entries(0).boundLo.poke(0x8000.U); config.entries(0).boundHi.poke(0x8008.U)
    }
    dut.io.tlbCsr.priv.imode.poke(0.U); dut.io.tlbCsr.priv.dmode.poke(3.U)
    dut.io.tlbCsr.pmm.mseccfg.poke(3.U)
    dut.io.csrCtrl.cache_error_enable.poke(true.B)
    dut.reset.poke(true.B); dut.clock.step(10); cycle += 10
    dut.reset.poke(false.B); dut.clock.step(20); cycle += 20
    val deadline = cycle + 100
    while (cycle < deadline && (tail < 0 || cycle < tail + 10)) {
      val offer = if (!issued.contains(first.rob)) Some(first)
        else if (!issued.contains(other.rob) && cycle >= issued(first.rob) + 2) Some(other) else None
      dut.io.request.valid.poke(offer.nonEmpty.B)
      offer.foreach { source =>
        zero(dut.io.request.bits)
        dut.io.request.bits.src(0).poke(((BigInt(0xabcd) << 48) | source.address).U)
        dut.io.request.bits.uop.robIdx.value.poke(source.rob.U)
        dut.io.request.bits.uop.lqIdx.value.poke(lq(source).U)
        dut.io.request.bits.uop.pdest.poke((source.rob + 1).U)
        dut.io.request.bits.uop.fuType.poke(FuType.ldu.U); dut.io.request.bits.uop.fuOpType.poke(3.U)
        dut.io.request.bits.uop.rfWen.poke(true.B)
        dut.io.request.bits.uop.firstUop.poke(true.B); dut.io.request.bits.uop.lastUop.poke(true.B)
        dut.io.request.bits.uop.fdiNotTrusted.foreach(_.poke(true.B))
        dut.io.request.bits.isFirstIssue.poke(true.B)
      }
      zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
      dut.io.cacheBankConflict.poke(false.B)
      attempts.find(_.accepted + 2 == cycle).foreach { item =>
        dut.io.cacheResponse.valid.poke(true.B); dut.io.cacheResponse.bits.id.poke(item.id.U)
        dut.io.cacheResponse.bits.data.poke((item.source.data | (item.source.data << 64)).U)
        val originalFirst = (item.source eq first) && item.accepted == issued(first.rob)
        dut.io.cacheBankConflict.poke(originalFirst.B)
      }
      attempts.find(_.accepted + 3 == cycle).foreach { item =>
        dut.io.cacheResponse.bits.data_delayed.poke((item.source.data | (item.source.data << 64)).U)
      }
      if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) {
        val source = offer.get
        assert(!issued.contains(source.rob)); issued(source.rob) = cycle
        record("issue", s"\"rob\":${source.rob},\"lq\":${lq(source)},\"effective\":${source.address}")
      }
      if (bool(dut.io.cacheRequest.valid)) {
        val source = byRob(uint(dut.io.phases.s0Rob).toInt)
        attempts += CacheAttempt(source, cycle, uint(dut.io.cacheRequest.bits.id))
        record("cache-attempt", s"\"rob\":${source.rob},\"ordinal\":${attempts.count(_.source eq source)}")
      }
      if (bool(dut.io.phases.fastReplay.valid)) {
        fastReplays += 1
        dut.io.phases.fastReplay.bits.uop.robIdx.value.expect(first.rob.U)
        dut.io.phases.fastReplay.bits.uop.lqIdx.value.expect(0.U)
        if (enabled) dut.io.phases.fastReplay.bits.fullva.expect(first.address.U)
        record("fast-replay", s"\"rob\":${first.rob}")
      }
      if (bool(dut.io.phases.tlbResponse.valid) && uint(dut.io.phases.tlbResponse.bits.memidx.idx) == lq(other)) {
        dut.io.phases.tlbResponse.bits.fullva.expect(other.address.U)
        otherTlbResponse = true
        record("other-tlb-response", s"\"lq\":${lq(other)},\"fullva\":${other.address}")
      }
      if (bool(dut.io.phases.s1) && bool(dut.io.phases.s1NoQuery)) {
        assert(fastReplays == 1 && otherTlbResponse, "Missing real prior TLB owner or fast replay")
        dut.io.phases.s1Rob.expect(first.rob.U)
        dut.io.phases.s2.expect(true.B); dut.io.phases.s2Rob.expect(other.rob.U)
        dut.io.phases.tlbResponse.valid.expect(false.B)
        dut.io.phases.tlbResponse.bits.memidx.idx.expect(lq(other).U)
        dut.io.phases.tlbResponse.bits.fullva.expect(other.address.U)
        if (enabled) {
          dut.io.phases.permissionRequest.valid.expect(true.B)
          dut.io.phases.permissionRequest.bits.address.expect(first.address.U)
          dut.io.phases.permissionRequest.bits.tag.expect(permissionTag(first).U)
        }
        overlap = true
        record("noquery-owned-address-overlap", s"\"retry_rob\":${first.rob},\"retry_fullva\":${first.address}," +
          s"\"other_rob\":${other.rob},\"stale_tlb_fullva\":${other.address},\"tlb_valid\":false")
      }
      if (bool(dut.io.phases.permissionResponse.valid)) {
        val source = pendingPermission.getOrElse(throw new AssertionError("Unowned overlap permission response"))
        dut.io.phases.permissionResponse.bits.request.tag.expect(permissionTag(source).U)
        dut.io.phases.permissionResponse.bits.request.address.expect(source.address.U)
        dut.io.phases.permissionResponse.bits.outcome.expect((if (source.allow) 0 else 1).U)
        dut.io.phases.permissionConsumed.expect(true.B)
        permissionResponses(source.rob) += 1; pendingPermission = None
      }
      if (bool(dut.io.phases.permissionRequest.valid)) {
        val source = byRob(uint(dut.io.phases.s1Rob).toInt)
        assert(pendingPermission.isEmpty)
        dut.io.phases.permissionRequest.bits.tag.expect(permissionTag(source).U)
        dut.io.phases.permissionRequest.bits.address.expect(source.address.U)
        dut.io.phases.permissionRequest.bits.sourcePrivilege.expect(0.U)
        dut.io.phases.permissionRequest.bits.sourceVirtual.expect(false.B)
        dut.io.phases.permissionRequest.bits.notTrusted.expect(true.B)
        permissionRequests(source.rob) += 1; pendingPermission = Some(source)
      }
      if (bool(dut.io.result.valid) && bool(dut.io.result.ready)) {
        val source = byRob(uint(dut.io.result.bits.uop.robIdx.value).toInt)
        assert(completions.add(source.rob), "Overlap source completed twice")
        dut.io.result.bits.uop.lqIdx.value.expect(lq(source).U)
        val denied = enabled && !source.allow
        dut.io.result.bits.data.expect((if (denied) BigInt(0) else source.data).U)
        dut.io.result.bits.uop.rfWen.expect((!denied).B); dut.io.result.bits.uop.fpWen.expect(false.B)
        dut.io.result.bits.uop.exceptionVec.zipWithIndex.foreach { case (bit, index) =>
          bit.expect((denied && index == 24).B)
        }
        if (denied) {
          dut.io.result.bits.uop.fdiException.get.tval.expect(source.address.U)
          dut.io.result.bits.uop.fdiException.get.reason.expect(2.U)
          dut.io.phases.ld2Cancel.expect(true.B)
        }
        record("completion", s"\"rob\":${source.rob},\"denied\":$denied,\"data\":${uint(dut.io.result.bits.data)}")
        if (completions.size == 2) tail = cycle
      }
      dut.io.phases.replay.valid.expect(false.B)
      dut.io.phases.split.expect(false.B); dut.io.phases.l2l.expect(false.B)
      dut.clock.step(); cycle += 1
    }
    assert(issued.size == 2 && completions.size == 2 && fastReplays == 1 && overlap)
    assert(attempts.count(_.source eq first) == 2 && attempts.count(_.source eq other) == 1)
    assert(permissionRequests(first.rob) == (if (enabled) 2 else 0) && permissionRequests(other.rob) == (if (enabled) 1 else 0))
    assert(permissionResponses == permissionRequests && pendingPermission.isEmpty)
    dut.io.phases.replayAllocated.expect(0.U)
    dut.io.phases.permissionResponse.valid.expect(false.B); dut.io.result.valid.expect(false.B)
    record("complete")
    cycle
  }

  it should "publish only the allowed load and complete a denied load without data" in {
    val root = Paths.get(sys.props.getOrElse("l01.runRoot", ".")).toAbsolutePath
    Files.createDirectories(root)
    require(Paths.get("").toRealPath() == root.toRealPath(), "Run from the dedicated load test directory")
    val (base, _, _) = top.ArgParser.parse(Array("--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
      "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
    }
    val options = p(DebugOptionsKey)
    require(!p(XSCoreParamsKey).EnableLoadToLoadForward,
      "This fixture preserves the production profile's disabled load-to-load forwarding")
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    test(new FDILoadPermissionHarness(remainingBoundaries = scenario == "remaining")).withAnnotations(Seq(VerilatorBackendAnnotation,
      TargetDirAnnotation(s"load-$scenario-${if (enabled) "on" else "off"}"))) { dut =>
      val sources = Seq(Source(2, BigInt(0x8000), BigInt("13579bdf2468ace0", 16)),
        Source(3, BigInt(0x8010), BigInt("c35ad19ef026b478", 16)))
      val byRob = sources.map(s => s.rob -> s).toMap
      val accepted = mutable.Set.empty[Int]
      val completed = mutable.Set.empty[Int]
      val permissionRequests = mutable.Set.empty[Int]
      val pendingCache = mutable.ArrayBuffer.empty[CacheAttempt]
      val events = mutable.ArrayBuffer.empty[String]
      var cycle = 0
      var offered: Option[Source] = None
      var deniedS2 = 0
      var deniedS3 = 0
      var replacements = 0
      def record(kind: String, source: Source, detail: String = ""): Unit =
        events += s"{\"cycle\":$cycle,\"event\":\"$kind\",\"rob\":${source.rob},\"address\":${source.address}${if (detail.isEmpty) "" else "," + detail}}"

      zero(dut.io.request.bits); dut.io.request.valid.poke(false.B)
      zero(dut.io.tlbCsr); zero(dut.io.csrCtrl); zero(dut.io.sfence)
      dut.io.trigger.foreach(zero)
      zero(dut.io.redirect.bits); dut.io.redirect.valid.poke(false.B)
      zero(dut.io.pmp); zero(dut.io.cacheResponse.bits)
      dut.io.cacheResponse.valid.poke(false.B); dut.io.cacheReady.poke(true.B)
      dut.io.cacheBankConflict.poke(false.B); dut.io.cacheNack.poke(false.B)
      dut.io.result.ready.poke(true.B)
      dut.io.ptw.req.foreach(_.ready.poke(true.B))
      zero(dut.io.ptw.resp.bits); dut.io.ptw.resp.valid.poke(false.B)
      zero(dut.io.dchannel); zero(dut.io.mshrData)
      dut.io.mshrValid.poke(false.B); dut.io.mshrMatch.poke(false.B); dut.io.mshrCorrupt.poke(false.B)
      zero(dut.io.tlbHint.bits); dut.io.tlbHint.valid.poke(false.B)
      zero(dut.io.lqHead)
      for (port <- Seq(dut.io.sq, dut.io.sbuffer, dut.io.ubuffer)) {
        zero(port.forwardMaskFast); zero(port.forwardMask); zero(port.forwardData)
        port.dataInvalid.poke(false.B); port.matchInvalid.poke(false.B); port.addrInvalid.poke(false.B)
      }
      dut.io.sq.dataInvalidFast.poke(false.B)
      zero(dut.io.sq.dataInvalidSqIdx); zero(dut.io.sq.addrInvalidSqIdx)
      dut.io.config.foreach { config =>
        zero(config)
        config.sourcePrivilege.poke(0.U)
        config.policy.uEnable.poke(true.B)
        config.entries(0).entryValid.poke(true.B)
        config.entries(0).readAllowed.poke(true.B)
        config.entries(0).boundLo.poke(0x8000.U)
        config.entries(0).boundHi.poke(0x8008.U)
      }
      // Source HU is deliberately independent from the bare M data translation.
      dut.io.tlbCsr.priv.imode.poke(0.U)
      dut.io.tlbCsr.priv.dmode.poke(3.U)
      dut.io.csrCtrl.cache_error_enable.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(10)
      dut.reset.poke(false.B); dut.clock.step(20)

      def edge(): Unit = {
        zero(dut.io.cacheResponse.bits)
        dut.io.cacheResponse.valid.poke(false.B)
        pendingCache.find(_.accepted + 2 == cycle).foreach { item =>
          dut.io.cacheResponse.valid.poke(true.B)
          dut.io.cacheResponse.bits.data.poke((item.source.data | (item.source.data << 64)).U)
          dut.io.cacheResponse.bits.id.poke(item.id.U)
        }
        pendingCache.find(_.accepted + 3 == cycle).foreach { item =>
          dut.io.cacheResponse.bits.data_delayed.poke((item.source.data | (item.source.data << 64)).U)
        }
        if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) {
          val source = offered.get
          assert(accepted.add(source.rob), "Duplicate instruction acceptance")
          record("issue", source)
        }
        if (bool(dut.io.cacheRequest.valid)) {
          val source = offered.getOrElse(throw new AssertionError("Unexpected replay in the minimum"))
          assert(uint(dut.io.cacheRequest.bits.vaddr) == source.address)
          pendingCache += CacheAttempt(source, cycle, uint(dut.io.cacheRequest.bits.id))
          record("cache-request", source)
        }
        if (bool(dut.io.phases.permissionRequest.valid)) {
          val address = uint(dut.io.phases.permissionRequest.bits.address)
          val source = sources.find(_.address == address).get
          assert(permissionRequests.add(source.rob), "Duplicate permission request in a one-attempt case")
          dut.io.phases.permissionRequest.bits.sourcePrivilege.expect(0.U)
          dut.io.phases.permissionRequest.bits.sourceVirtual.expect(false.B)
          dut.io.phases.permissionRequest.bits.notTrusted.expect(true.B)
          dut.io.phases.permissionRequest.bits.sizeLog2.expect(3.U)
          dut.io.phases.permissionRequest.bits.pc.expect(0.U)
          record("permission", source)
        }
        if (bool(dut.io.phases.permissionRequest.valid) && bool(dut.io.phases.permissionConsumed))
          replacements += 1
        if (bool(dut.io.phases.s2)) {
          val source = byRob(uint(dut.io.phases.s2Rob).toInt)
          dut.io.phases.s2Address.expect(source.address.U)
          dut.io.phases.s2TlbMiss.expect(false.B)
          if (enabled && !source.allow) {
            deniedS2 += 1
            dut.io.phases.blocked.expect(true.B)
            dut.io.phases.fast.expect(false.B)
            dut.io.phases.ld1Cancel.expect(true.B)
            dut.io.cacheS2Kill.expect(true.B)
          }
        }
        dut.io.phases.split.expect(false.B)
        dut.io.phases.replay.valid.expect(false.B)
        if (bool(dut.io.result.valid) && bool(dut.io.result.ready)) {
          val source = byRob(uint(dut.io.result.bits.uop.robIdx.value).toInt)
          assert(accepted(source.rob) && completed.add(source.rob), "Unknown or duplicate completion")
          val denied = enabled && !source.allow
          dut.io.result.bits.uop.rfWen.expect((!denied).B)
          dut.io.result.bits.uop.fpWen.expect(false.B)
          dut.io.result.bits.data.expect((if (denied) BigInt(0) else source.data).U)
          val vector = dut.io.result.bits.uop.exceptionVec.map(_.peek().litValue)
          assert(vector.zipWithIndex.forall { case (bit, index) => bit == (if (denied && index == 24) 1 else 0) },
            s"Unexpected exception vector at source ${source.rob}: $vector")
          if (denied) {
            deniedS3 += 1
            dut.io.result.bits.uop.fdiException.get.tval.expect(source.address.U)
            dut.io.result.bits.uop.fdiException.get.reason.expect(2.U)
            dut.io.phases.ld2Cancel.expect(true.B)
            dut.io.phases.s3.expect(true.B)
            dut.io.phases.l2l.expect(false.B)
          }
          record("completion", source, s"\"denied\":$denied")
        }
        dut.clock.step(); cycle += 1
      }
      try {
        for (source <- sources) {
          zero(dut.io.request.bits)
          dut.io.request.bits.src(0).poke(source.address.U)
          dut.io.request.bits.uop.robIdx.value.poke(source.rob.U)
          dut.io.request.bits.uop.lqIdx.value.poke(source.rob.U)
          dut.io.request.bits.uop.pdest.poke((source.rob + 1).U)
          dut.io.request.bits.uop.fuType.poke(FuType.ldu.U)
          dut.io.request.bits.uop.fuOpType.poke(3.U)
          dut.io.request.bits.uop.rfWen.poke(true.B)
          dut.io.request.bits.uop.firstUop.poke(true.B)
          dut.io.request.bits.uop.lastUop.poke(true.B)
          dut.io.request.bits.uop.fdiNotTrusted.foreach(_.poke(true.B))
          dut.io.request.bits.isFirstIssue.poke(true.B)
          offered = Some(source)
          dut.io.request.valid.poke(true.B)
          val end = cycle + 80
          while (!accepted(source.rob) && cycle < end) edge()
          assert(accepted(source.rob), "Load input failed to accept")
          dut.io.request.valid.poke(false.B)
          offered = None
          if (scenario == "minimal") {
            while (!completed(source.rob) && cycle < end) edge()
            assert(completed(source.rob), "Load completion failed to arrive")
            for (_ <- 0 until 4) edge()
          }
        }
        val completionDeadline = cycle + 80
        while (completed.size < sources.size && cycle < completionDeadline) edge()
        if (scenario == "full") for (_ <- 0 until 4) edge()
        assert(completed.size == 2 && pendingCache.size == 2)
        assert(permissionRequests.size == (if (enabled) 2 else 0))
        assert(deniedS2 == (if (enabled) 1 else 0) && deniedS3 == (if (enabled) 1 else 0))
        if (scenario == "full" && enabled) assert(replacements > 0, "Consecutive opposite permissions never replaced a consumed slot")
        println(s"L01_LOCAL_MINIMAL_PASS enabled=$enabled cycles=$cycle sources=2 permission=${permissionRequests.size} " +
          s"denied=$deniedS3 tlb=production replay=production cache=external-fixed-cycle")
        if (scenario != "minimal") full(dut, events, cycle)
      } finally {
        Files.write(root.resolve("load-permission-events.jsonl"), events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
