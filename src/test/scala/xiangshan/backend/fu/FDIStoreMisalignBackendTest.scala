// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import xiangshan.backend.fu.FuConfig.StaCfg

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend
import xiangshan._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.MemoryOpConstants
import xiangshan.frontend.{BranchPredictionBundle, FtqPtr}
import xiangshan.mem.SqPtr

class FDIStoreMisalignBackendTest extends AnyFlatSpec {
  behavior of "Nonaligned scalar stores through the production Backend"

  private val enabled = sys.env.getOrElse("L05_FDI_ENABLED",
    throw new IllegalArgumentException("L05_FDI_ENABLED must be explicit")).toBoolean
  private val base = BigInt("80000000", 16)
  private val mask64 = (BigInt(1) << 64) - 1
  private val storePc = BigInt(0x4004)
  private val payload = BigInt("78695a4b3c2d1e0f", 16)
  private case class Pointer(flag: Boolean, value: BigInt)
  private case class Owner(rob: Pointer, sq: Pointer)
  private case class PtwReply(due: Int, vpn: BigInt, sq: BigInt)
  private case class Trial(name: String, offset: Int, bytes: Int = 8, floating: Boolean = false,
    translated: Boolean = false, repair: Boolean = false, supervisor: Boolean = false,
    pageFault: Boolean = false, pmpFault: Boolean = false, cancel: Boolean = false) {
    val address: BigInt = base + offset
    val privilege: Int = if (supervisor) 1 else 0
    def denied(generation: Int): Boolean = enabled && generation == 0 &&
      (repair || supervisor || pageFault || pmpFault)
    def cause(generation: Int): Option[Int] =
      if (pageFault) Some(15) else if (pmpFault) Some(7)
      else if (denied(generation)) Some(if (supervisor) 25 else 24) else None
    def tval: BigInt = if (pageFault || pmpFault) base + 4096 else address
  }

  private def bool(value: Bool): Boolean = value.peek().litToBoolean
  private def uint(value: UInt): BigInt = value.peek().litValue
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
  private def addi(rd: Int, rs: Int, immediate: Int): BigInt =
    (BigInt(immediate & 4095) << 20) | (BigInt(rs) << 15) | (BigInt(rd) << 7) | 0x13
  private def slli(rd: Int, shift: Int): BigInt =
    (BigInt(shift) << 20) | (BigInt(rd) << 15) | (BigInt(1) << 12) | (BigInt(rd) << 7) | 0x13

  private class Driver(dut: FDIStoreMisalignBackendHarness, trial: Trial,
    events: mutable.ArrayBuffer[String]) {
    private val view = dut.splitView
    private var cycle = 410
    private var generation = 0
    private var setupPc = BigInt(0x1000)
    private var csrResponses = 0
    private var traps = 0
    private var hsTraps = 0
    private var debugEntries = 0
    private var reasonEffects = 0
    private var prohibitEpcWrite = false
    private val fetches = mutable.Map.empty[Pointer, BigInt]
    private val redirects = mutable.Map.empty[(Pointer, BigInt), BigInt]
    private val locations = mutable.Map.empty[(Pointer, BigInt), Int]
    private val robOwners = mutable.Map.empty[Pointer, Int]
    private val owners = mutable.Map.empty[Int, Owner]
    private val addressAttempts = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val dataIssues = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val dataCompletions = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val completions = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val retirements = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val fragments = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val fragmentSuccess = mutable.Map.empty[Int, Set[BigInt]].withDefaultValue(Set.empty)
    private val mabWrites = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val enqueues = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val canceled = mutable.Set.empty[Int]
    private val robFaults = mutable.Set.empty[Int]
    private val metadataReplies = Seq.fill(dut.memory.metadata.size)(mutable.Queue.empty[Int])
    private val cacheReplies = mutable.Queue.empty[(Int, BigInt)]
    private val ptwReplies = mutable.ArrayBuffer.empty[PtwReply]
    private val requestedPages = mutable.Set.empty[BigInt]
    private val answeredPages = mutable.Set.empty[BigInt]
    private val bytePublications = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    private val cacheByteWrites = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    private val actualBytes = mutable.Map.empty[BigInt, Int]
    private val expectedBytes = (0 until trial.bytes).map { byte =>
      (trial.address + byte) -> ((payload >> (8 * byte)) & 255).toInt
    }.toMap
    private var emptyTokens = 0
    private var permissionRequests = 0
    private var permissionResponses = 0
    private var heldResident = false
    private var olderLocation: Option[(Pointer, BigInt)] = None
    private var olderRob: Option[Pointer] = None
    private var floatingLocation: Option[(Pointer, BigInt)] = None
    private var floatingRob: Option[Pointer] = None
    private var floatingRetired = false

    private def pointer(value: RobPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    private def pointer(value: SqPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    private def pointer(value: FtqPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    private def setPointer(value: FtqPtr, p: Pointer): Unit = {
      value.flag.poke(p.flag.B); value.value.poke(p.value.U)
    }
    private def owner(uop: DynInst): (Int, Owner) = {
      val key = Owner(pointer(uop.robIdx), pointer(uop.sqIdx))
      val occurrence = robOwners.getOrElse(key.rob, throw new AssertionError(s"Unknown store ROB $key"))
      assert(owners.get(occurrence).contains(key), "STA/STD/MAB changed the allocated ROB/SQ identity")
      (occurrence, key)
    }
    private def record(kind: String, fields: String = ""): Unit =
      events += s"{\"case\":\"${trial.name}\",\"cycle\":$cycle,\"event\":\"$kind\"" +
        (if (fields.isEmpty) "}" else "," + fields + "}")
    private def identity(occurrence: Int, key: Owner): String =
      s"\"generation\":$occurrence,\"pc\":$storePc,\"rob\":${key.rob.value},\"rob_flag\":${key.rob.flag}," +
        s"\"sq\":${key.sq.value},\"sq_flag\":${key.sq.flag}"
    private def faultBits(vector: Vec[Bool]): Set[Int] =
      StaCfg.exceptionOut.filter(index => bool(vector(index))).toSet

    private def edge(): Unit = {
      dut.memory.metadata.zipWithIndex.foreach { case (port, lane) =>
        port.resp.valid.poke(metadataReplies(lane).headOption.contains(cycle).B)
        if (metadataReplies(lane).headOption.contains(cycle)) metadataReplies(lane).dequeue()
      }
      dut.memory.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (cacheReplies.headOption.exists(_._1 == cycle)) {
        val (_, id) = cacheReplies.dequeue()
        dut.memory.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        dut.memory.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
      }
      dut.memory.ptw.resp.valid.poke(false.B)
      ptwReplies.filter(_.due == cycle).foreach { pending =>
        val reply = dut.memory.ptw.resp.bits
        zero(reply)
        val sector = reply.s1.pteidx.length
        val index = (pending.vpn % sector).toInt
        val pf = trial.pageFault && pending.vpn == (base >> 12) + 1
        reply.memidx.is_st.poke(true.B); reply.memidx.idx.poke(pending.sq.U)
        reply.s1.entry.tag.poke((pending.vpn / sector).U)
        reply.s1.entry.level.foreach(_.poke(0.U))
        // Map the PMP-denied high page elsewhere so the original low-page
        // request is allowed; only its actual high-fragment translation fails.
        val physicalVpn = pending.vpn + (if (trial.pmpFault && pending.vpn == (base >> 12) + 1) 2 else 0)
        reply.s1.entry.ppn.poke((physicalVpn / sector).U)
        reply.s1.entry.v.poke((!pf).B)
        reply.s1.entry.perm.foreach { perm =>
          perm.r.poke(true.B); perm.w.poke(true.B); perm.a.poke(true.B)
          perm.d.poke(true.B); perm.u.poke((trial.privilege == 0).B)
        }
        reply.s1.addr_low.poke(index.U); reply.s1.pf.poke(pf.B)
        reply.s1.valididx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        reply.s1.pteidx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        reply.s1.ppn_low.zipWithIndex.foreach { case (value, i) =>
          value.poke((if (i == index) physicalVpn % sector else BigInt(i)).U)
        }
        dut.memory.ptw.resp.valid.poke(true.B); dut.memory.ptw.resp.ready.expect(true.B)
        answeredPages += pending.vpn
        record("ptw-response", s"\"vpn\":${pending.vpn},\"sq\":${pending.sq},\"page_fault\":$pf")
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady) && !bool(dut.io.ifuRedirect.valid))
        fetches(pointer(dut.io.fetchRequest.bits.ftqIdx)) = uint(dut.io.fetchRequest.bits.startAddr)
      if (bool(dut.io.csrResponse.valid)) csrResponses += 1
      if (prohibitEpcWrite && bool(dut.io.legalCSRWrite.valid))
        assert(uint(dut.io.legalCSRWrite.bits) != 0x341, "The repaired store must return to the original trap EPC")
      if (bool(dut.io.frontendRedirect.valid)) {
        val redirect = dut.io.frontendRedirect.bits
        redirects((pointer(redirect.ftqIdx), uint(redirect.ftqOffset))) = uint(redirect.cfiUpdate.target)
        record("frontend-redirect", s"\"target\":${uint(redirect.cfiUpdate.target)}")
      }
      (Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq).foreach { port =>
        if (bool(port.valid) && bool(port.ready))
          locations.get((pointer(port.bits.ftqPtr), uint(port.bits.ftqOffset))).foreach { occurrence =>
            record("frontend-store", s"\"generation\":$occurrence,\"pc\":${uint(port.bits.pc)}," +
              s"\"raw\":${uint(port.bits.instr)},\"ftq\":${uint(port.bits.ftqPtr.value)}," +
              s"\"ftq_flag\":${bool(port.bits.ftqPtr.flag)},\"offset\":${uint(port.bits.ftqOffset)}," +
              s"\"source_mode\":${uint(dut.io.mode)},\"tag\":${port.bits.fdiNotTrusted.map(bool).getOrElse(false)}")
          }
      }
      dut.io.robEnq.foreach { port =>
        if (bool(port.valid) && floatingLocation.contains((pointer(port.bits.ftqIdx), uint(port.bits.ftqOffset))))
          floatingRob = Some(pointer(port.bits.robIdx))
        if (bool(port.valid) && olderLocation.contains((pointer(port.bits.ftqIdx), uint(port.bits.ftqOffset))))
          olderRob = Some(pointer(port.bits.robIdx))
        if (bool(port.valid)) locations.get((pointer(port.bits.ftqIdx), uint(port.bits.ftqOffset))).foreach { occurrence =>
          val rob = pointer(port.bits.robIdx)
          robOwners.get(rob).foreach(previous => assert(previous == occurrence,
            "A new store aliases a live ROB owner"))
          robOwners(rob) = occurrence
          record("rob-allocation", s"\"generation\":$occurrence,\"rob\":${rob.value},\"rob_flag\":${rob.flag}")
        }
      }
      view.allocated.foreach { port =>
        if (bool(port.valid)) locations.get((pointer(port.bits.ftqPtr), uint(port.bits.ftqOffset))).foreach { occurrence =>
          val key = Owner(pointer(port.bits.robIdx), pointer(port.bits.sqIdx))
          owners.get(occurrence).foreach(old => assert(old == key, "One store received multiple SQ identities"))
          owners(occurrence) = key
          robOwners.get(key.rob).foreach(old => assert(old == occurrence))
          robOwners(key.rob) = occurrence
          record("sq-allocation", identity(occurrence, key))
        }
      }
      if (bool(dut.memory.dispatchRedirect.valid)) {
        val boundary = pointer(dut.memory.dispatchRedirect.bits.robIdx)
        val self = uint(dut.memory.dispatchRedirect.bits.level).testBit(0)
        owners.foreach { case (occurrence, key) =>
          val after = if (key.rob.flag == boundary.flag) key.rob.value > boundary.value else key.rob.value <= boundary.value
          if (after || self && key.rob == boundary) {
            canceled += occurrence
            record("store-cancel", identity(occurrence, key))
          }
        }
      }
      dut.memory.issue.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) {
          val (occurrence, key) = owner(port.bits.uop)
          assert(!canceled(occurrence))
          assert(((uint(port.bits.src(0)) + uint(port.bits.uop.imm)) & mask64) == trial.address)
          addressAttempts(occurrence) += 1
          record("sta-issue", identity(occurrence, key) + s",\"lane\":$lane,\"address\":${trial.address}")
        }
      }
      dut.memory.dataIssue.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) {
          val (occurrence, key) = owner(port.bits.uop)
          assert(!canceled(occurrence)); dataIssues(occurrence) += 1
          if (trial.floating) assert(floatingRetired, "FP store data arrived without its real retired FMV producer")
          assert(dataIssues(occurrence) == 1, "A store's STD issued more than once")
          assert((uint(port.bits.src(0)) & ((BigInt(1) << (trial.bytes * 8)) - 1)) ==
            (payload & ((BigInt(1) << (trial.bytes * 8)) - 1)))
          record("std-issue", identity(occurrence, key) + s",\"lane\":$lane,\"data\":${uint(port.bits.src(0))},\"floating\":${trial.floating}")
        }
      }
      view.standardData.foreach { port =>
        if (bool(port.valid)) {
          val (occurrence, key) = owner(port.bits.uop)
          dataCompletions(occurrence) += 1; assert(dataCompletions(occurrence) == 1)
          record("std-completion", identity(occurrence, key))
        }
      }
      view.enqueued.foreach { port =>
        if (bool(port.valid)) {
          val (occurrence, key) = owner(port.bits.uop)
          assert(!canceled(occurrence)); enqueues(occurrence) += 1
          record("mab-enqueue", identity(occurrence, key) + s",\"fullva\":${uint(port.bits.fullva)}")
        }
      }
      if (bool(view.request.valid)) {
        val (occurrence, key) = owner(view.request.bits.uop)
        assert(!canceled(occurrence)); fragments(occurrence) += 1
        record("fragment-request", identity(occurrence, key) +
          s",\"fullva\":${uint(view.request.bits.fullva)},\"vaddr\":${uint(view.request.bits.vaddr)},\"final\":${bool(view.request.bits.isFinalSplit)}")
      }
      if (bool(view.response.valid)) {
        val (occurrence, key) = owner(view.response.bits.uop)
        val faults = faultBits(view.response.bits.uop.exceptionVec)
        if (!bool(view.response.bits.need_rep) && faults.isEmpty)
          fragmentSuccess(occurrence) += uint(view.response.bits.vaddr)
        record("fragment-response", identity(occurrence, key) +
          s",\"retry\":${bool(view.response.bits.need_rep)},\"vaddr\":${uint(view.response.bits.vaddr)},\"faults\":[${faults.toSeq.sorted.mkString(",")}]")
      }
      if (bool(view.writeback.valid)) {
        val (occurrence, key) = owner(view.writeback.bits.uop)
        mabWrites(occurrence) += 1; assert(mabWrites(occurrence) == 1)
        record("mab-writeback", identity(occurrence, key))
      }
      dut.memory.completion.foreach { port =>
        if (bool(port.valid)) {
          val (occurrence, key) = owner(port.bits.uop)
          assert(!trial.cancel && !canceled(occurrence), "Canceled store produced a terminal writeback")
          completions(occurrence) += 1; assert(completions(occurrence) == 1)
          val faults = faultBits(port.bits.uop.exceptionVec)
          trial.cause(occurrence) match {
            case Some(cause) => assert(faults(cause), s"Missing terminal cause $cause in $faults")
            case None => assert(faults.isEmpty, s"Allowed store completed with $faults")
          }
          if (trial.denied(occurrence) && !trial.pageFault && !trial.pmpFault) {
            port.bits.uop.fdiException.get.tval.expect(trial.address.U)
            port.bits.uop.fdiException.get.reason.expect(3.U)
          }
          record("store-writeback", identity(occurrence, key) + s",\"faults\":[${faults.toSeq.sorted.mkString(",")}]")
        }
      }
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid) && floatingRob.contains(pointer(port.bits))) {
          assert(!floatingRetired); floatingRetired = true
          record("fp-producer-retire", s"\"rob\":${uint(port.bits.value)},\"rob_flag\":${bool(port.bits.flag)}")
          floatingRob = None; floatingLocation = None
        }
        if (bool(port.valid)) robOwners.get(pointer(port.bits)).foreach { occurrence =>
          assert(!trial.cancel && !canceled(occurrence) && trial.cause(occurrence).isEmpty)
          assert(completions(occurrence) == 1)
          retirements(occurrence) += 1; assert(retirements(occurrence) == 1)
          record("store-retire", s"\"generation\":$occurrence")
        }
      }
      dut.memory.metadata.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.req.valid) && bool(port.req.ready)) {
          port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
          metadataReplies(lane).enqueue(cycle + 2)
        }
      }
      dut.memory.phases.foreach { phase =>
        if (bool(phase.permissionRequest.valid)) {
          permissionRequests += 1
          phase.permissionRequest.bits.address.expect(trial.address.U)
          phase.permissionRequest.bits.sizeLog2.expect((if (trial.bytes == 4) 2 else 3).U)
          phase.permissionRequest.bits.operation.expect(1.U)
        }
        if (bool(phase.permissionResponse.valid)) permissionResponses += 1
      }
      dut.memory.ptw.req.foreach { port =>
        if (bool(port.valid) && bool(port.ready)) {
          assert(trial.translated); port.bits.memidx.is_st.expect(true.B)
          val vpn = uint(port.bits.vpn)
          assert(Set(base >> 12, (base >> 12) + 1)(vpn))
          assert(owners.values.exists(_.sq.value == uint(port.bits.memidx.idx)))
          if (requestedPages.add(vpn)) ptwReplies += PtwReply(cycle + 12, vpn, uint(port.bits.memidx.idx))
          record("ptw-request", s"\"vpn\":$vpn,\"sq\":${uint(port.bits.memidx.idx)}")
        }
      }
      dut.memory.publication.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.fire)) {
          val sq = pointer(dut.memory.publicationSq(lane))
          val candidates = owners.filter(_._2.sq == sq).toSeq
          assert(candidates.size == 1, "SQ publication lost its original store owner")
          val (occurrence, key) = candidates.head
          if (bool(port.bits.vecValid)) {
            assert(!trial.cancel && !canceled(occurrence) && trial.cause(occurrence).isEmpty,
              "A faulting or canceled store published target bytes")
            assert(completions(occurrence) == 1 && fragmentSuccess(occurrence).size == 2,
              "Store bytes became visible before both fragments and the original completion")
            val wordBase = uint(port.bits.addr) & ~BigInt(15)
            val mask = uint(port.bits.mask)
            assert(mask != 0)
            for (byte <- 0 until port.bits.mask.getWidth if mask.testBit(byte)) {
              val address = wordBase + byte
              assert(expectedBytes.contains(address), "Publication overwrote a sentinel byte")
              assert(((uint(port.bits.data) >> (8 * byte)) & 255).toInt == expectedBytes(address))
              bytePublications(address) += 1; assert(bytePublications(address) == 1, "One store byte was published twice")
            }
            record("sq-publication", identity(occurrence, key) + s",\"address\":${uint(port.bits.addr)},\"mask\":$mask")
          } else { emptyTokens += 1; record("empty-sq-token", identity(occurrence, key)) }
        }
        assert(bool(dut.memory.writes(lane).valid) == (bool(port.fire) && bool(port.bits.vecValid)))
      }
      val write = dut.memory.cacheWrite.req
      if (bool(write.valid) && bool(write.ready)) {
        write.bits.cmd.expect(MemoryOpConstants.M_XWR)
        val line = uint(write.bits.addr)
        val byteCount = write.bits.mask.getWidth
        val wanted = expectedBytes.filter { case (address, _) => line <= address && address < line + byteCount }
        assert(wanted.nonEmpty && !trial.cancel && trial.cause(generation).isEmpty)
        val mask = wanted.keys.foldLeft(BigInt(0))((bits, address) => bits.setBit((address - line).toInt))
        write.bits.mask.expect(mask.U)
        wanted.foreach { case (address, value) =>
          assert(bytePublications(address) == 1)
          assert(((uint(write.bits.data) >> (8 * (address - line).toInt)) & 255).toInt == value)
          cacheByteWrites(address) += 1; assert(cacheByteWrites(address) == 1)
          actualBytes(address) = value
        }
        cacheReplies.enqueue((cycle + 3, uint(write.bits.id)))
        record("cache-line-write", s"\"line\":$line,\"mask\":$mask,\"data\":${uint(write.bits.data)}")
      }
      if (!trial.cancel && bool(dut.io.robException.valid) && !bool(dut.io.robException.bits.isInterrupt)) {
        dut.io.robException.bits.pc.expect(storePc.U)
        assert(robFaults.add(generation))
        record("rob-store-exception", s"\"generation\":$generation,\"faults\":[${faultBits(dut.io.robException.bits.exceptionVec).toSeq.sorted.mkString(",")}]")
      }
      if (bool(dut.memory.reasonEffect)) { reasonEffects += 1; record("freason-effect") }
      if (bool(dut.io.mEntry)) { traps += 1; record("machine-entry") }
      if (bool(dut.io.hsEntry)) { hsTraps += 1; record("supervisor-entry") }
      if (bool(dut.io.debugEntry)) { debugEntries += 1; record("debug-entry") }
      assert(!bool(dut.memory.uncache.req.valid))
      dut.clock.step(); cycle += 1
    }

    private def until(label: String, limit: Int = 6000)(condition: => Boolean): Unit = {
      val stop = cycle + limit
      while (!condition && cycle < stop) edge()
      assert(condition, s"${trial.name}: timed out waiting for $label at $cycle")
    }
    private def idle(count: Int): Unit = (0 until count).foreach(_ => edge())
    private def prediction(stage: BranchPredictionBundle, pc: BigInt, ptr: Pointer): Unit = {
      zero(stage); stage.pc.foreach(_.poke(pc.U)); stage.valid.foreach(_.poke(true.B)); setPointer(stage.ftq_idx, ptr)
    }
    private def packet(pc: BigInt, instructions: Seq[BigInt], storeIndex: Option[Int] = None): Pointer = {
      val lanes = Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq
      require(instructions.nonEmpty && instructions.size <= lanes.size)
      until("frontend recovery drains")(!bool(dut.io.backendRedirect.valid) && !bool(dut.io.frontendRedirect.valid))
      val ptr = pointer(dut.io.ftqNext)
      fetches -= ptr
      redirects.keys.filter(_._1 == ptr).toList.foreach(redirects.remove)
      locations.keys.filter(_._1 == ptr).toList.foreach(locations.remove)
      storeIndex.foreach(index => locations((ptr, BigInt(2 * index))) = generation)
      if (trial.cancel && storeIndex.contains(1)) olderLocation = Some((ptr, BigInt(0)))
      if (instructions.size == 1 && Set(BigInt("f0000053", 16), BigInt("f2000053", 16))(
        instructions.head & BigInt("fe00007f", 16))) {
        floatingLocation = Some((ptr, BigInt(0)))
        record("fp-producer-packet", s"\"pc\":$pc,\"raw\":${instructions.head}")
      }
      zero(dut.io.bpu.resp.bits); prediction(dut.io.bpu.resp.bits.s1, pc, ptr)
      dut.io.bpu.resp.valid.poke(true.B); until("BPU ready")(bool(dut.io.bpu.resp.ready)); edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1); prediction(dut.io.bpu.resp.bits.s2, pc, ptr); edge()
      zero(dut.io.bpu.resp.bits.s2); prediction(dut.io.bpu.resp.bits.s3, pc, ptr); edge()
      zero(dut.io.bpu.resp.bits.s3)
      until("real FTQ fetch")(fetches.contains(ptr)); assert(fetches(ptr) == pc)
      zero(dut.io.predecode.bits); setPointer(dut.io.predecode.bits.ftqIdx, ptr)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (value, index) => value.poke((pc + 2 * index).U) }
      instructions.indices.foreach { index =>
        dut.io.predecode.bits.pd(2 * index).valid.poke(true.B)
        dut.io.predecode.bits.instrRange(2 * index).poke(true.B)
      }
      dut.io.predecode.bits.ftqOffset.poke((2 * (instructions.size - 1)).U)
      dut.io.predecode.bits.target.poke((pc + 4 * instructions.size).U)
      dut.io.predecode.valid.poke(true.B); edge(); dut.io.predecode.valid.poke(false.B)
      var consumed = 0
      val stop = cycle + 2000
      while (consumed < instructions.size && cycle < stop) {
        lanes.zipWithIndex.foreach { case (lane, slot) =>
          val index = consumed + slot
          zero(lane.bits); lane.valid.poke((index < instructions.size).B)
          if (index < instructions.size) {
            val sourcePc = pc + 4 * index
            lane.bits.instr.poke(instructions(index).U); lane.bits.pc.poke(sourcePc.U)
            lane.bits.pd.valid.poke(true.B); lane.bits.trigger.poke(TriggerAction.None)
            // Host trust follows the configured main interval [0x1000,0x3000).
            lane.bits.fdiNotTrusted.foreach(_.poke((enabled && uint(dut.io.mode) != 3 &&
              (sourcePc < 0x1000 || sourcePc >= 0x3000)).B))
            setPointer(lane.bits.ftqPtr, ptr); lane.bits.ftqOffset.poke((2 * index).U)
            lane.bits.isLastInFtqEntry.poke((index == instructions.size - 1).B)
          }
        }
        val count = (0 until instructions.size - consumed).takeWhile(index => bool(lanes(index).ready)).size
        edge(); consumed += count
      }
      assert(consumed == instructions.size, "The complete packet must reach the real decode input")
      lanes.foreach(_.valid.poke(false.B)); ptr
    }
    private def instruction(bits: BigInt): Unit = {
      packet(setupPc, Seq(bits)); setupPc += 0x40; idle(24)
    }
    private def setRegister(rd: Int, value: BigInt): Unit = {
      val significant = (0 until 8).reverse.map(i => ((value >> (8 * i)) & 255).toInt).dropWhile(_ == 0)
      val bytes = if (significant.isEmpty) Seq(0) else significant
      instruction(addi(rd, 0, bytes.head))
      bytes.tail.foreach { byte => instruction(slli(rd, 8)); if (byte != 0) instruction(addi(rd, rd, byte)) }
    }
    private def writeCsr(address: Int, value: BigInt, flush: Boolean = false): Unit = {
      setRegister(31, value)
      val pc = setupPc; setupPc += 0x40
      val before = csrResponses
      val ptr = packet(pc, Seq((BigInt(address) << 20) | (BigInt(31) << 15) | 0x1073))
      until("real CSR response")(csrResponses > before)
      if (flush) until("real CSR redirect")(redirects.get((ptr, BigInt(0))).contains(pc + 4))
      idle(20)
    }
    private def program(): Unit = {
      val operation = if (trial.floating) 0x27 else 0x23
      val store = (BigInt(10) << 20) | (BigInt(1) << 15) |
        (BigInt(if (trial.bytes == 4) 2 else 3) << 12) | operation
      if (trial.cancel) {
        val divide = (BigInt(1) << 25) | (BigInt(3) << 20) | (BigInt(2) << 15) |
          (BigInt(5) << 12) | (BigInt(20) << 7) | 0x33
        packet(storePc - 4, Seq(divide, store), Some(1))
      } else packet(storePc, Seq(store), Some(0))
    }
    private def finishMemory(expectWrite: Boolean): Unit = {
      until("store queue and MAB drain")(bool(dut.memory.queueEmpty) && !bool(view.full))
      dut.memory.forceFlush.poke(true.B); dut.memory.cacheWrite.req.ready.poke(true.B)
      until("Sbuffer and cache acknowledgements drain") {
        bool(dut.memory.flushDone) && bool(dut.memory.bufferEmpty) && cacheReplies.isEmpty
      }
      dut.memory.forceFlush.poke(false.B); idle(24)
      val expected = if (expectWrite) expectedBytes else Map.empty[BigInt, Int]
      assert(actualBytes.toMap == expected && bytePublications.keySet.toSet == expected.keySet)
      assert(cacheByteWrites.keySet.toSet == expected.keySet)
      for (address <- trial.address - 16 until trial.address + trial.bytes + 16)
        assert(actualBytes.getOrElse(address, 0x5a) == expected.getOrElse(address, 0x5a))
      dut.memory.slots.foreach(_.allocated.expect(false.B))
      assert(metadataReplies.forall(_.isEmpty) && permissionRequests == permissionResponses)
    }
    def run(): Unit = {
      zeroInputs(dut.io); zeroInputs(dut.memory)
      dut.io.fetchReady.poke(true.B)
      dut.memory.metadata.foreach(_.req.ready.poke(true.B))
      dut.memory.ptw.req.foreach(_.ready.poke(true.B))
      dut.memory.uncache.req.ready.poke(true.B)
      dut.memory.cacheWrite.req.ready.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(10); dut.reset.poke(false.B); dut.clock.step(400)
      record("case-start", s"\"enabled\":$enabled,\"address\":${trial.address},\"bytes\":${trial.bytes},\"floating\":${trial.floating}")
      writeCsr(0x305, 0x6000); writeCsr(0x105, 0x7000)
      writeCsr(0x3b0, (if (trial.pmpFault) base + 8192 else BigInt(1) << 32) >> 2)
      writeCsr(0x3a0, 15)
      if (enabled) {
        writeCsr(0x30c, 1); writeCsr(0x10c, 1)
        writeCsr(0xbc5, 0x1000, flush = true); writeCsr(0xbc6, 0x3000, flush = true)
        writeCsr(0x9e2, 0x1000, flush = true); writeCsr(0x9e3, 0x3000, flush = true)
        writeCsr(0x890, base, flush = true)
        val limit = if (trial.supervisor) base + 64 else base + 4096
        writeCsr(0x891, if (trial.denied(0)) limit else base + 8192, flush = true)
        writeCsr(0x880, 9, flush = true)
        writeCsr(0xbc4, if (trial.supervisor) 1 else 2, flush = true)
        writeCsr(0x8b3, 7, flush = true)
      }
      if (trial.translated) writeCsr(0x180, (BigInt(8) << 60) | 1, flush = true)
      if (trial.supervisor) writeCsr(0x302, BigInt(1) << 25)
      setRegister(1, trial.address); setRegister(10, payload)
      if (trial.cancel) { setRegister(2, BigInt("7123456789abcdef", 16)); setRegister(3, 7) }
      if (trial.floating) {
        // Move actual integer data through the production FP register path.
        writeCsr(0x300, BigInt(1) << 13)
        instruction((BigInt(if (trial.bytes == 4) 0x78 else 0x79) << 25) |
          (BigInt(10) << 15) | (BigInt(10) << 7) | 0x53)
        until("real FMV producer retirement")(floatingRetired)
        // Distinct integer-bank data makes an incorrect STD source-bank choice
        // fail the byte oracle even though the architectural register number matches.
        setRegister(10, BigInt("2132435465768798", 16))
      }
      writeCsr(0x341, if (trial.cancel) storePc - 4 else storePc)
      writeCsr(0x300, (BigInt(trial.privilege) << 11) | (BigInt(1) << 13))
      val ptr = packet(setupPc, Seq(BigInt(0x30200073))); setupPc += 0x40
      until("MRET and source context") {
        uint(dut.io.mode) == trial.privilege && redirects.get((ptr, BigInt(0))).contains(if (trial.cancel) storePc - 4 else storePc)
      }
      idle(16); dut.memory.sourcePrivilege.expect(trial.privilege.U)
      program()
      if (trial.cancel) {
        until("real MAB resident behind older DIV") {
          owners.get(0).exists { key => bool(view.full) && pointer(view.resident.rob) == key.rob &&
            pointer(view.resident.sq) == key.sq && dataIssues(0) == 1 && completions(0) == 0 &&
            bool(dut.io.robHead.valid) && olderRob.contains(pointer(dut.io.robHead.robIdx)) &&
            bool(dut.io.robHead.interruptSafe) && !bool(dut.io.robHead.writebacked) }
        }
        heldResident = true
        record("resident-halt-request", identity(0, owners(0)))
        dut.io.interrupts.debug.poke(true.B)
        until("actual Debug entry cancels resident Store")(debugEntries == 1 && canceled(0))
        dut.io.interrupts.debug.poke(false.B)
        finishMemory(expectWrite = false)
        dut.io.dpc.expect((storePc - 4).U)
        assert((uint(dut.io.dcsr) & 3) == 0 && ((uint(dut.io.dcsr) >> 6) & 7) == 3)
        assert(heldResident && retirements(0) == 0 && completions(0) == 0 && mabWrites(0) == 0)
        assert(traps + hsTraps == 0 && reasonEffects == 0)
      } else if (trial.cause(0).nonEmpty) {
        until("precise store trap")(traps + hsTraps == 1)
        until("matching handler FTQ redirect")(redirects.values.exists(_ == (if (trial.supervisor) BigInt(0x7000) else BigInt(0x6000))))
        idle(20)
        val cause = trial.cause(0).get
        if (trial.supervisor) {
          dut.io.scause.expect(cause.U); dut.io.sepc.expect(storePc.U); dut.memory.supervisorTval.expect(trial.tval.U)
          assert(hsTraps == 1 && traps == 0)
        } else {
          dut.io.mcause.expect(cause.U); dut.io.mepc.expect(storePc.U); dut.io.mtval.expect(trial.tval.U)
        }
        assert(completions(0) == 1 && retirements(0) == 0 && robFaults(0))
        val fdiWinner = cause == 24 || cause == 25
        dut.memory.reason.expect((if (!enabled) 0 else if (fdiWinner) 3 else 7).U)
        assert(reasonEffects == (if (enabled && fdiWinner) 1 else 0))
        finishMemory(expectWrite = false)
        record("precise-trap", s"\"cause\":$cause,\"epc\":$storePc,\"tval\":${trial.tval},\"freason\":${uint(dut.memory.reason)}")
        if (trial.repair) {
          val old = owners(0)
          // The drained old incarnation no longer owns recycled ROB/SQ pointers.
          // Preserve its counters while allowing the real handler to allocate.
          robOwners.clear(); owners.clear(); locations.clear()
          prohibitEpcWrite = true; setupPc = 0x6000
          writeCsr(0x891, base + 8192, flush = true)
          dut.io.mepc.expect(storePc.U)
          generation = 1
          dut.memory.cacheWrite.req.ready.poke(false.B)
          val returned = packet(setupPc, Seq(BigInt(0x30200073))); setupPc += 0x40
          until("MRET retries the original EPC") {
            uint(dut.io.mode) == 0 && redirects.get((returned, BigInt(0))).contains(storePc)
          }
          idle(16); program()
          until("repaired original Store retires once")(retirements(1) == 1)
          assert(completions(1) == 1 && mabWrites(1) == 1)
          record("retry-owner", identity(1, owners(1)) + s",\"old_rob\":${old.rob.value},\"old_sq\":${old.sq.value}")
          finishMemory(expectWrite = true)
          assert(traps == 1 && reasonEffects == 1)
          record("same-epc-retry", identity(1, owners(1)) + ",\"mepc_rewritten\":false")
        }
      } else {
        until("allowed Store retires once")(retirements(0) == 1)
        assert(completions(0) == 1 && mabWrites(0) == 1 && enqueues(0) >= 1)
        finishMemory(expectWrite = true)
        assert(traps + hsTraps == 0 && reasonEffects == 0)
      }
      assert(dataIssues(0) == 1 && dataCompletions(0) == 1 && addressAttempts(0) >= 1)
      if (trial.translated) assert(answeredPages == Set(base >> 12, (base >> 12) + 1))
      if (!enabled) assert(permissionRequests == 0 && permissionResponses == 0)
      record("case-complete", s"\"generations\":${generation + 1},\"traps\":${traps + hsTraps}," +
        s"\"byte_writes\":${actualBytes.size},\"empty_tokens\":$emptyTokens,\"debug_entries\":$debugEntries")
    }
  }

  it should "preserve store ownership, precise traps and atomic byte publication" in {
    val root = Paths.get(sys.props("l05.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run in the dedicated L05 Backend evidence directory")
    val path = root.resolve(if (enabled) "store-misalign-backend-enabled" else "store-misalign-backend-disabled")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDIStoreMisalignBackendHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    val trials = Seq(
      Trial("integer-split", 13),
      Trial("floating-word-cross-line", 63, bytes = 4, floating = true),
      Trial("floating-double-cross-page", 4093, floating = true, translated = true)) ++
      (if (enabled) Seq(Trial("hu-bound-repair", 4093, repair = true),
        Trial("hs-delegated-denial", 63, bytes = 4, floating = true, supervisor = true)) else Seq.empty) ++
      Seq(Trial("high-page-pf", 4093, floating = true, translated = true, pageFault = true),
        Trial("high-page-pmp", 4093, translated = true, pmpFault = true),
        Trial("resident-debug-cancel", 4093, cancel = true))
    val events = mutable.ArrayBuffer.empty[String]
    try {
      simulation.runElaboratedModule(module) { elaborated =>
        trials.foreach(trial => new Driver(elaborated.wrapped, trial, events).run())
      }
      println(s"L05_BACKEND_PASS enabled=$enabled cases=${trials.size} actual_backend=true actual_mab=true actual_fp=true")
    } finally {
      Files.write(path.resolve("store-misalign-backend-events.jsonl"),
        events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
  }
}
