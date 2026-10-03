// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{BranchPredictionBundle, FtqPtr}

class FDICSRRecoveryBackendTest extends AnyFlatSpec {
  behavior of "FDI CSR production backend recovery"

  private val scenario = sys.env.getOrElse("C07_BACKEND_SCENARIO", "full")
  require(Set("minimal", "full", "trace").contains(scenario))

  // These literals define the oracle independently of the DUT address maps.
  private val addresses = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e1, 0x9e2, 0x9e3, 0x880) ++
    (0x890 to 0x8af) ++ (0x8b0 to 0x8b3) ++ (0x8c0 to 0x8c8)
  private val projection = Seq(0xbc4, 0x9e1, 0xbc5, 0xbc6, 0x9e2, 0x9e3, 0x880) ++
    (0x890 to 0x8af) ++ Seq(0x8b0, 0x8b1, 0x8b2, 0x8b3, 0x8c8) ++ (0x8c0 to 0x8c7)
  private val frontendAddresses = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e2, 0x9e3)
  private val controlAddresses = Seq(0xbc4, 0x8c8) ++ (0x8c0 to 0x8c7) ++ Seq(0x8b0, 0x8b1, 0x8b2)
  private val memoryAddresses = Seq(0xbc4, 0x880) ++ (0x890 to 0x8af)
  private val allBits = (BigInt(1) << 64) - 1
  private val takenBranchInstruction = BigInt("04000063", 16) // BEQ x0, x0, +64.
  private def backing(address: Int): Int = if (address == 0x9e1) 0xbc4 else address
  private def mask(address: Int): BigInt = address match {
    case 0xbc4 => BigInt(0x7ff)
    case 0x9e1 => BigInt(0x7c2)
    case 0x880 => BigInt("bbbbbbbbbbbbbbbb", 16)
    case 0x8c8 => BigInt("0001000100010001", 16)
    case 0x8b3 => BigInt(7)
    case 0x8b0 | 0x8b1 | 0x8b2 => allBits
    case _ => allBits ^ 7
  }
  private case class Pointer(flag: Boolean, value: BigInt)
  private case class Fetch(pointer: Pointer, offset: Int)
  private case class Request(address: Int, value: Int, write: Boolean) {
    require(value >= 0 && value < 32)
    val fdi: Boolean = addresses.contains(address)
    val instruction: BigInt = (BigInt(address) << 20) |
      (if (write) (BigInt(value) << 15) | (BigInt(5) << 12) else BigInt(2) << 12) |
      (BigInt(3) << 7) | 0x73
  }
  private class Access(val pc: BigInt, val request: Request, val ptr: Pointer, val sourceFetch: Fetch,
    val uid: BigInt, val c0: Int, val old: BigInt, val finalWord: BigInt) {
    var effect = -1
    var response = -1
    var writeback = -1
    var post = -1
    var robFlush = -1
    var robRedirect = -1
    var backendRedirect = -1
    var ftqRedirect = -1
    var recoveryFetch = -1
    var recoveryPointer: Option[Pointer] = None
    var recoveryAccept = -1
    var retire = -1
    var retireCount = 0
  }
  private class BranchRecovery(val pc: BigInt, val target: BigInt) {
    var robRedirect = -1
    var backendRedirect = -1
    var ftqRedirect = -1
    var recoveryFetch = -1
    var recoveryPointer: Option[Pointer] = None
    var retire = -1
  }

  private class Driver(val dut: FDICSRRecoveryBackendHarness) {
    var cycles = 0
    val events = mutable.ArrayBuffer.empty[String]
    val planned = mutable.Map.empty[BigInt, Request]
    val accesses = mutable.Map.empty[BigInt, Access]
    val byPointer = mutable.Map.empty[Pointer, Access]
    val byUid = mutable.Map.empty[BigInt, Access]
    val allocations = mutable.Map.empty[Fetch, Pointer]
    val discarded = mutable.Set.empty[Fetch]
    val fetchPCs = mutable.Map.empty[Fetch, BigInt]
    val fetches = mutable.Map.empty[Pointer, (Int, BigInt)]
    val retiredPointers = mutable.Set.empty[Pointer]
    val retirementCycles = mutable.Map.empty[Pointer, Int]
    val waitForwardSources = mutable.Map.empty[BigInt, BigInt]
    val owner = mutable.Map.from(addresses.map(backing).distinct.map(_ -> BigInt(0)))
    val control = mutable.Map.from(controlAddresses.map(_ -> BigInt(0)))
    val frontend = mutable.Map.from(frontendAddresses.map(_ -> BigInt(0)))
    val memory = mutable.Map.from(memoryAddresses.map(_ -> BigInt(0)))
    private var transport = Vector.fill[Option[(Int, BigInt)]](2)(None)
    private var pendingEffect: Option[Access] = None
    private var pendingRecovery: Option[Access] = None
    private var expectedBranch: Option[BranchRecovery] = None
    private var traceSourcePC: Option[BigInt] = None
    private var traceReleaseCycle = -1
    private var traceBlockedSamples = 0
    private var traceReadyBlockedSamples = 0
    private var previousTraceBlocked: Option[Boolean] = None

    def bool(value: Bool): Boolean = value.peek().litToBoolean
    def uint(value: UInt): BigInt = value.peek().litValue
    def pointer(value: RobPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    def pointer(value: FtqPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    def fetch(value: FtqPtr, offset: UInt): Fetch = Fetch(pointer(value), uint(offset).toInt)
    def zero(data: Data): Unit = data match {
      case value: Bool => value.poke(false.B)
      case value: UInt => value.poke(0.U)
      case value: SInt => value.poke(0.S)
      case value: Vec[_] => value.foreach(zero)
      case value: Record => value.elements.values.foreach(zero)
      case other => throw new IllegalArgumentException(s"Unsupported input ${other.getClass.getName}")
    }
    def setPointer(field: FtqPtr, value: Pointer): Unit = {
      field.flag.poke(value.flag.B)
      field.value.poke(value.value.U)
    }
    def record(kind: String, fields: String = ""): Unit =
      events += s"{\"cycle\":$cycles,\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    private def recordAccess(kind: String, access: Access, observed: String = ""): Unit = {
      val f = access.sourceFetch
      val cfg = Seq(dut.recovery.owner(0), dut.recovery.control(0),
        dut.recovery.frontend(0), dut.recovery.memory(0)).map(uint)
      record(kind, s"\"uid\":${access.uid},\"pc\":${access.pc},\"address\":${access.request.address}," +
        s"\"rob\":${access.ptr.value},\"rob_flag\":${access.ptr.flag},\"ftq\":${f.pointer.value}," +
        s"\"ftq_flag\":${f.pointer.flag},\"ftq_offset\":${f.offset},\"maincfg_owner_control_frontend_memory\":[${cfg.mkString(",")}]" +
        (if (observed.isEmpty) "" else "," + observed))
    }
    private def updateMirror(state: mutable.Map[Int, BigInt], token: Option[(Int, BigInt)]): Unit =
      token.foreach { case (address, data) =>
        val key = backing(address)
        if (state.contains(key)) state(key) = data
      }
    def checkState(): Unit = {
      addresses.zipWithIndex.foreach { case (address, index) =>
        dut.recovery.owner(index).expect((owner(backing(address)) & mask(address)).U)
      }
      for ((keys, expected, actual) <- Seq(
        (controlAddresses, control, dut.recovery.control),
        (frontendAddresses, frontend, dut.recovery.frontend),
        (memoryAddresses, memory, dut.recovery.memory)); (key, index) <- keys.zipWithIndex) {
        actual(index).expect(expected(key).U)
      }
    }
    private def requireConverged(access: Access): Unit = {
      assert(cycles >= access.effect + 3, "Recovery preceded remote mirror visibility")
      for (state <- Seq(control, frontend, memory); (address, word) <- state) {
        assert(word == owner(address), s"Recovery saw an old mirror at address $address")
      }
      checkState()
    }
    private def branchSource(branch: BranchRecovery): Option[(Fetch, Pointer)] =
      allocations.find { case (id, _) => fetchPCs(id) == branch.pc }
    private def requireBranch(ptr: Pointer, id: Fetch): BranchRecovery = {
      val branch = expectedBranch.getOrElse(throw new AssertionError(s"Unexpected redirect for $ptr"))
      assert(branchSource(branch).contains((id, ptr)),
        s"Redirect does not match the expected branch source PC ${branch.pc}")
      branch
    }
    def edge(): Unit = {
      checkState()
      assert(!bool(dut.io.robException.valid), s"Unexpected trap at cycle $cycles")
      traceSourcePC.foreach { pc =>
        val stalled = bool(dut.io.traceStall)
        val blocked = bool(dut.io.traceBlocked)
        assert(bool(dut.io.traceEnable))
        if (stalled) assert(blocked, "The established external trace stall lost its real ROB backpressure")
        if (!previousTraceBlocked.contains(blocked)) {
          record("trace-block-change", s"\"source_pc\":$pc,\"stall\":$stalled,\"blocked\":$blocked")
          previousTraceBlocked = Some(blocked)
        }
        if (blocked) {
          dut.io.robRetire.foreach(_.valid.expect(false.B))
          dut.recovery.retired.foreach(_.valid.expect(false.B))
          accesses.get(pc).filter(_.retireCount == 0).foreach { access =>
            val headMatches = bool(dut.io.robHead.valid) && pointer(dut.io.robHead.robIdx) == access.ptr
            val headReady = headMatches && bool(dut.io.robHead.writebacked)
            traceBlockedSamples += 1
            if (headReady) traceReadyBlockedSamples += 1
            recordAccess("trace-blocked", access,
              s"\"stall\":$stalled,\"blocked\":true,\"head_matches\":$headMatches," +
                s"\"head_writebacked\":$headReady,\"retire_count\":${access.retireCount}," +
                s"\"release_cycle\":$traceReleaseCycle")
          }
        }
      }
      dut.io.robEnq.foreach { port =>
        if (bool(port.valid) && bool(port.bits.first)) {
          val id = fetch(port.bits.ftqIdx, port.bits.ftqOffset)
          assert(!discarded(id), s"Discarded frontend input later allocated a ROB entry: $id")
          assert(fetchPCs.contains(id), s"ROB allocation has no frontend transaction: $id")
          assert(!allocations.contains(id), s"Frontend transaction allocated twice: $id")
          val ptr = pointer(port.bits.robIdx)
          allocations(id) = ptr
          retiredPointers -= ptr
          retirementCycles -= ptr
          record("rob-allocation", s"\"pc\":${fetchPCs(id)},\"rob\":${ptr.value},\"flag\":${ptr.flag}")
        }
      }
      dut.recovery.packets.foreach { packet =>
        if (bool(packet.valid)) {
          val kind = uint(packet.kind).toInt
          if (kind == 2) {
            val pc = uint(packet.pc)
            assert(planned.contains(pc), s"Unplanned FDI request at PC $pc")
            assert(!accesses.contains(pc), s"Repeated C0 for PC $pc")
            val request = planned(pc)
            val ptr = Pointer(bool(packet.robFlag), uint(packet.robIdx))
            val old = owner(backing(request.address)) & mask(request.address)
            val word = if (request.address == 0x9e1) {
              (owner(0xbc4) & (allBits ^ mask(0x9e1))) | (BigInt(request.value) & mask(0x9e1))
            } else BigInt(request.value) & mask(request.address)
            val sourceFetch = allocations.collectFirst {
              case (id, rob) if rob == ptr && fetchPCs(id) == pc => id
            }.getOrElse(throw new AssertionError("CSR acceptance has no allocated FTQ identity"))
            val access = new Access(pc, request, ptr, sourceFetch, uint(packet.uid), cycles, old, word)
            assert(access.uid != 0 && !byUid.contains(access.uid))
            packet.csrAddress.expect(request.address.U)
            packet.instruction.expect(request.instruction.U)
            packet.writeNeeded.expect(request.write.B)
            packet.permitted.expect(true.B)
            assert(allocations.exists { case (id, rob) => rob == ptr && fetchPCs(id) == pc })
            waitForwardSources.get(pc).foreach { olderPc =>
              val older = allocations.find { case (id, _) => fetchPCs(id) == olderPc }.get._2
              assert(retirementCycles.get(older).exists(_ < cycles),
                "FDI C0 must occur strictly after the older instruction's actual retirement")
              record("wait-forward", s"\"older_pc\":$olderPc,\"retire\":${retirementCycles(older)},\"c0\":$cycles")
            }
            accesses(pc) = access
            byPointer(ptr) = access
            byUid(access.uid) = access
            if (request.write) {
              assert(pendingEffect.isEmpty)
              pendingEffect = Some(access)
            }
            recordAccess("c0", access)
          } else if (kind == 3) {
            val access = byUid(uint(packet.uid))
            assert(access.post < 0 && cycles == access.c0 + 2)
            packet.actualWrite.expect(access.request.write.B)
            packet.responseIllegal.expect(false.B)
            packet.responseVirtual.expect(false.B)
            access.post = cycles
            recordAccess("post", access)
          } else {
            assert(kind == 1, s"Unexpected FDI trap/cancel packet kind $kind")
          }
          projection.zipWithIndex.foreach { case (address, index) =>
            packet.words(index).expect((owner(backing(address)) & mask(address)).U)
          }
        }
      }
      if (bool(dut.recovery.request.valid) && uint(dut.recovery.request.bits.address) == 0x180) {
        val request = dut.recovery.request.bits
        val pc = uint(request.pc)
        assert(planned.contains(pc) && !accesses.contains(pc))
        val expected = planned(pc)
        assert(expected.address == 0x180 && expected.write && expected.value == 0)
        val ptr = pointer(request.robIdx)
        val sourceFetch = allocations.collectFirst {
          case (id, rob) if rob == ptr && fetchPCs(id) == pc => id
        }.getOrElse(throw new AssertionError("SATP acceptance has no allocated FTQ identity"))
        val access = new Access(pc, expected, ptr, sourceFetch, uint(request.uid), cycles, 0, 0)
        assert(access.uid != 0 && !byUid.contains(access.uid) && pendingEffect.isEmpty)
        accesses(pc) = access
        byPointer(ptr) = access
        byUid(access.uid) = access
        pendingEffect = Some(access)
        recordAccess("satp-c0", access)
      }
      val effect = pendingEffect.filter(_.c0 + 1 == cycles)
      dut.recovery.effects.expect(effect.filter(_.request.fdi).map(a => BigInt(1) << addresses.indexOf(a.request.address))
        .getOrElse(BigInt(0)).U)
      dut.recovery.bus.w.valid.expect(effect.nonEmpty.B)
      effect.foreach { access =>
        dut.recovery.bus.w.bits.addr.expect(access.request.address.U)
        dut.recovery.bus.w.bits.data.expect(access.finalWord.U)
        assert(access.effect < 0)
        access.effect = cycles
        pendingEffect = None
        recordAccess("c1-effect", access)
      }
      if (bool(dut.recovery.response.valid)) {
        val response = dut.recovery.response.bits
        val access = byPointer(pointer(response.robIdx))
        assert(access.response < 0 && cycles >= access.c0 + 1)
        response.flushPipe.expect(access.request.write.B)
        response.illegal.expect(false.B)
        response.virtualFault.expect(false.B)
        response.data.expect(access.old.U)
        access.response = cycles
        recordAccess("response", access)
      }
      if (bool(dut.recovery.writeback.valid)) {
        val wb = dut.recovery.writeback.bits
        byPointer.get(pointer(wb.robIdx)).foreach { access =>
          assert(access.writeback < 0)
          wb.flushPipe.expect(access.request.write.B)
          access.writeback = cycles
          recordAccess("writeback", access)
        }
      }
      if (bool(dut.io.robFlush.valid)) {
        val flush = dut.io.robFlush.bits
        val access = byPointer(pointer(flush.robIdx))
        assert(access.request.write && access.robFlush < 0 && access.writeback >= 0)
        assert(cycles >= access.writeback + 4, "ROB flush violated the registered writeback lower bound")
        flush.level.expect(xiangshan.RedirectLevel.flushAfter)
        access.robFlush = cycles
        requireConverged(access)
        recordAccess("rob-flush-after", access)
      }
      if (bool(dut.recovery.robRedirect.valid)) {
        val redirect = dut.recovery.robRedirect.bits
        redirect.level.expect(xiangshan.RedirectLevel.flushAfter)
        byPointer.get(pointer(redirect.robIdx)) match {
          case Some(access) =>
            assert(access.robRedirect < 0 && cycles > access.robFlush)
            access.robRedirect = cycles
            recordAccess("rob-redirect", access)
          case None =>
            val branch = requireBranch(pointer(redirect.robIdx), fetch(redirect.ftqIdx, redirect.ftqOffset))
            assert(branch.robRedirect < 0)
            branch.robRedirect = cycles
            record("branch-rob-redirect", s"\"pc\":${branch.pc},\"target\":${branch.target}")
        }
      }
      if (bool(dut.io.backendRedirect.valid)) {
        val redirect = dut.io.backendRedirect.bits
        byPointer.get(pointer(redirect.robIdx)) match {
          case Some(access) =>
            assert(access.backendRedirect < 0 && cycles >= access.robFlush + 6)
            redirect.cfiUpdate.target.expect((access.pc + 4).U)
            access.backendRedirect = cycles
            requireConverged(access)
            recordAccess("backend-recovery-pc", access, s"\"recovery_target\":${uint(redirect.cfiUpdate.target)}")
          case None =>
            val branch = requireBranch(pointer(redirect.robIdx), fetch(redirect.ftqIdx, redirect.ftqOffset))
            assert(branch.backendRedirect < 0 && branch.robRedirect >= 0)
            redirect.cfiUpdate.target.expect(branch.target.U)
            branch.backendRedirect = cycles
            record("branch-backend-redirect", s"\"pc\":${branch.pc},\"target\":${branch.target}")
        }
      }
      if (bool(dut.io.frontendRedirect.valid)) {
        val redirect = dut.io.frontendRedirect.bits
        byPointer.get(pointer(redirect.robIdx)) match {
          case Some(access) =>
            assert(access.ftqRedirect < 0 && cycles >= access.backendRedirect)
            redirect.cfiUpdate.target.expect((access.pc + 4).U)
            access.ftqRedirect = cycles
            assert(pendingRecovery.isEmpty)
            pendingRecovery = Some(access)
            requireConverged(access)
            recordAccess("ftq-recovery-pc", access, s"\"recovery_target\":${uint(redirect.cfiUpdate.target)}")
          case None =>
            val branch = requireBranch(pointer(redirect.robIdx), fetch(redirect.ftqIdx, redirect.ftqOffset))
            assert(branch.ftqRedirect < 0 && branch.backendRedirect >= 0)
            redirect.cfiUpdate.target.expect(branch.target.U)
            branch.ftqRedirect = cycles
            record("branch-ftq-redirect", s"\"pc\":${branch.pc},\"target\":${branch.target}")
        }
      }
      dut.recovery.retired.foreach { retired =>
        if (bool(retired.valid)) {
          val ptr = Pointer(bool(retired.robFlag), uint(retired.robIdx))
          assert(!retiredPointers(ptr), s"Duplicate retirement for $ptr")
          retiredPointers += ptr
          retirementCycles(ptr) = cycles
          expectedBranch.filter(branch => branchSource(branch).exists(_._2 == ptr)).foreach { branch =>
            assert(branch.retire < 0 && cycles != branch.robRedirect)
            branch.retire = cycles
            record("branch-retire", s"\"pc\":${branch.pc},\"uid\":${uint(retired.uid)}")
          }
          byUid.get(uint(retired.uid)).foreach { access =>
            assert(access.ptr == ptr && access.retireCount == 0)
            if (access.request.write) assert(access.robRedirect >= 0 && cycles > access.robRedirect)
            access.retire = cycles
            access.retireCount += 1
            recordAccess("source-retire", access)
            if (traceSourcePC.contains(access.pc)) {
              assert(traceReleaseCycle >= 0 && cycles > traceReleaseCycle && !bool(dut.io.traceBlocked))
              recordAccess("trace-source-retire", access,
                s"\"stall\":${bool(dut.io.traceStall)},\"blocked\":${bool(dut.io.traceBlocked)}," +
                  s"\"release_cycle\":$traceReleaseCycle,\"blocked_samples\":$traceBlockedSamples," +
                  s"\"ready_blocked_samples\":$traceReadyBlockedSamples")
            }
          }
        }
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady) && !bool(dut.io.ifuRedirect.valid)) {
        val request = dut.io.fetchRequest.bits
        val pc = uint(request.startAddr)
        fetches(pointer(request.ftqIdx)) = (cycles, pc)
        pendingRecovery.foreach { access =>
          assert(pc == access.pc + 4 && cycles > access.ftqRedirect,
            "The first recovered fetch must use the FTQ recovery target")
          assert(access.recoveryFetch < 0)
          access.recoveryFetch = cycles
          access.recoveryPointer = Some(pointer(request.ftqIdx))
          requireConverged(access)
          recordAccess("recovery-fetch", access, s"\"fetch_start\":$pc,\"recovery_ftq\":${uint(request.ftqIdx.value)}," +
            s"\"recovery_ftq_flag\":${bool(request.ftqIdx.flag)}")
          pendingRecovery = None
        }
        expectedBranch.filter(branch => branch.ftqRedirect >= 0 && branch.recoveryFetch < 0).foreach { branch =>
          assert(pc == branch.target && cycles > branch.ftqRedirect,
            "The first branch recovery fetch must use the real redirect target")
          branch.recoveryFetch = cycles
          branch.recoveryPointer = Some(pointer(request.ftqIdx))
          record("branch-recovery-fetch", s"\"pc\":$pc")
        }
      }
      val token = effect.map(access => (access.request.address, access.finalWord))
      token.filter(t => addresses.contains(t._1)).foreach { case (address, word) => owner(backing(address)) = word }
      updateMirror(control, token)
      updateMirror(frontend, transport.head)
      updateMirror(memory, transport.head)
      transport = transport.tail :+ token
      dut.clock.step()
      cycles += 1
      checkState()
    }
    def idle(count: Int): Unit = (0 until count).foreach(_ => edge())
    def until(description: String, maximum: Int = 2000)(condition: => Boolean): Unit = {
      var count = 0
      while (!condition && count < maximum) { edge(); count += 1 }
      assert(condition, s"Timed out waiting for $description at cycle $cycles")
    }
    def reset(): Unit = {
      (Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq).foreach { lane =>
        lane.valid.poke(false.B)
        zero(lane.bits)
      }
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits)
      zero(dut.io.predecode)
      zero(dut.io.interrupts)
      dut.io.fetchReady.poke(true.B)
      dut.io.traceEnable.poke(false.B)
      dut.io.traceStall.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(10)
      dut.reset.poke(false.B)
      idle(400)
      dut.io.mode.expect(3.U)
    }
    private def prediction(stage: BranchPredictionBundle, pc: BigInt, ptr: Pointer): Unit = {
      zero(stage)
      stage.pc.foreach(_.poke(pc.U))
      stage.valid.foreach(_.poke(true.B))
      setPointer(stage.ftq_idx, ptr)
    }

    // This is the normal BPU/FTQ/IFU input protocol used by the delivery fixture.
    // A redirect target may start a block at any halfword-aligned PC.
    def enqueue(instructions: Seq[BigInt], pc: BigInt,
      recovered: Option[Access] = None, fetchStall: Int = 0,
      recoveredBranch: Option[BranchRecovery] = None): Seq[Fetch] = {
      val lanes = Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq
      require(instructions.nonEmpty && instructions.size <= lanes.size)
      require(instructions.size * 2 <= dut.io.predecode.bits.pd.length)
      require(recovered.isEmpty || recoveredBranch.isEmpty)
      val alreadyFetched = recovered.exists(_.recoveryFetch >= 0) || recoveredBranch.exists(_.recoveryFetch >= 0)
      val ptr = recovered.flatMap(_.recoveryPointer).orElse(recoveredBranch.flatMap(_.recoveryPointer))
        .getOrElse(pointer(dut.io.ftqNext))
      val ids = instructions.indices.map(i => Fetch(ptr, 2 * i))
      ids.zipWithIndex.foreach { case (id, i) =>
        fetchPCs(id) = pc + 4 * i
        allocations -= id
      }
      if (!alreadyFetched) fetches -= ptr
      recovered.foreach { access =>
        assert(if (alreadyFetched) pendingRecovery.isEmpty else pendingRecovery.contains(access),
          "Refetch must retain ownership from the observed FTQ redirect")
      }
      if (recovered.isEmpty) assert(pendingRecovery.isEmpty, "A previous recovery has not fetched its target")
      recoveredBranch.foreach(branch => assert(expectedBranch.contains(branch) && branch.ftqRedirect >= 0))
      if (!alreadyFetched) {
        dut.io.fetchReady.poke((fetchStall == 0).B)
        zero(dut.io.bpu.resp.bits)
        prediction(dut.io.bpu.resp.bits.s1, pc, ptr)
        dut.io.bpu.resp.valid.poke(true.B)
        until("BPU acceptance")(bool(dut.io.bpu.resp.ready))
        edge()
        dut.io.bpu.resp.valid.poke(false.B)
        zero(dut.io.bpu.resp.bits.s1)
        prediction(dut.io.bpu.resp.bits.s2, pc, ptr)
        edge()
        zero(dut.io.bpu.resp.bits.s2)
        prediction(dut.io.bpu.resp.bits.s3, pc, ptr)
        edge()
        zero(dut.io.bpu.resp.bits.s3)
        if (fetchStall > 0) {
          idle(fetchStall)
          assert(!fetches.contains(ptr), "Fetch was accepted while the external IFU interface was stalled")
          dut.io.fetchReady.poke(true.B)
        }
      }
      until("actual FTQ fetch")(fetches.contains(ptr))
      assert(fetches(ptr)._2 == pc, "FTQ fetch changed the requested starting PC")
      recovered.foreach(access => assert(access.recoveryFetch >= 0))
      zero(dut.io.predecode.bits)
      setPointer(dut.io.predecode.bits.ftqIdx, ptr)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (field, i) => field.poke((pc + 2 * i).U) }
      ids.zip(instructions).foreach { case (id, instruction) =>
        dut.io.predecode.bits.pd(id.offset).valid.poke(true.B)
        dut.io.predecode.bits.pd(id.offset).brType.poke(
          (if (instruction == takenBranchInstruction) xiangshan.frontend.BrType.branch else xiangshan.frontend.BrType.notCFI))
        dut.io.predecode.bits.instrRange(id.offset).poke(true.B)
      }
      dut.io.predecode.bits.ftqOffset.poke(ids.last.offset.U)
      dut.io.predecode.bits.target.poke((pc + 4 * instructions.size).U)
      dut.io.predecode.valid.poke(true.B)
      edge()
      dut.io.predecode.valid.poke(false.B)
      var consumed = 0
      var waited = 0
      while (consumed < instructions.size && waited < 2000) {
        lanes.zipWithIndex.foreach { case (lane, slot) =>
          val original = consumed + slot
          lane.valid.poke((original < instructions.size).B)
          zero(lane.bits)
          if (original < instructions.size) {
            lane.bits.instr.poke(instructions(original).U)
            lane.bits.pc.poke((pc + 4 * original).U)
            lane.bits.trigger.poke(xiangshan.TriggerAction.None)
            lane.bits.pd.valid.poke(true.B)
            lane.bits.pd.brType.poke(
              (if (instructions(original) == takenBranchInstruction) xiangshan.frontend.BrType.branch else xiangshan.frontend.BrType.notCFI))
            setPointer(lane.bits.ftqPtr, ptr)
            lane.bits.ftqOffset.poke(ids(original).offset.U)
            lane.bits.isLastInFtqEntry.poke((original == instructions.size - 1).B)
          }
        }
        val available = instructions.size - consumed
        val accepted = (0 until available).takeWhile(i => bool(lanes(i).ready)).size
        assert((0 until available).count(i => bool(lanes(i).ready)) == accepted)
        assert(!bool(dut.recovery.robRedirect.valid), "A packet was still being offered at recovery")
        if (accepted > 0) {
          recovered.foreach { access =>
            assert(access.recoveryAccept < 0 && cycles > access.recoveryFetch)
            access.recoveryAccept = cycles
            requireConverged(access)
            recordAccess("recovery-instruction-accept", access, s"\"accepted_pc\":${uint(lanes.head.bits.pc)}," +
              s"\"recovery_ftq\":${ptr.value},\"recovery_ftq_flag\":${ptr.flag}")
          }
          record("frontend-accept", s"\"pc\":${pc + 4 * consumed},\"count\":$accepted")
        }
        edge()
        consumed += accepted
        waited += 1
      }
      assert(consumed == instructions.size, "The frontend failed to accept the packet")
      lanes.foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
      ids
    }
    def awaitRetired(id: Fetch): Unit = until("instruction retirement") {
      allocations.get(id).exists(retiredPointers)
    }
    def finishWrite(pc: BigInt, fetchStall: Int = 0, recoveryInstruction: BigInt = 0x13): Access = {
      if (fetchStall > 0) dut.io.fetchReady.poke(false.B)
      until("source retirement and FTQ recovery") {
        accesses.get(pc).exists(a => a.retireCount == 1 && a.ftqRedirect >= 0)
      }
      val access = accesses(pc)
      // Let the external predictor consume the real redirect before its new prediction.
      idle(4)
      val recovered = enqueue(Seq(recoveryInstruction), pc + 4, Some(access), fetchStall).head
      awaitRetired(recovered)
      assert(access.effect == access.c0 + 1)
      if (access.request.fdi) assert(access.post == access.c0 + 2)
      else assert(access.request.address == 0x180 && access.post < 0)
      assert(access.response >= access.effect && access.writeback == access.response)
      assert(access.retireCount == 1 && access.recoveryAccept > access.recoveryFetch)
      record("access-summary", s"\"uid\":${access.uid},\"pc\":$pc," +
        s"\"c0\":${access.c0},\"effect\":${access.effect},\"response\":${access.response}," +
        s"\"rob_flush\":${access.robFlush},\"rob_redirect\":${access.robRedirect}," +
        s"\"backend_redirect\":${access.backendRedirect},\"ftq_redirect\":${access.ftqRedirect}," +
        s"\"fetch\":${access.recoveryFetch},\"accept\":${access.recoveryAccept},\"retire\":${access.retire}")
      access
    }
    def write(pc: BigInt, address: Int, value: Int, fetchStall: Int = 0): Unit = {
      val request = Request(address, value, write = true)
      planned(pc) = request
      enqueue(Seq(request.instruction), pc)
      finishWrite(pc, fetchStall)
    }
    def traceBackpressure(): Unit = {
      val pc = BigInt(0x8000)
      val request = Request(0xbc4, 1, write = true)
      assert(allocations.isEmpty && accesses.isEmpty, "The isolated trace scenario requires an empty ROB history")
      dut.io.robHead.valid.expect(false.B)
      dut.io.traceEnable.poke(true.B)
      dut.io.traceStall.poke(true.B)
      val stallCycle = cycles
      record("trace-stall", s"\"source_pc\":$pc,\"enabled\":true,\"stall\":true," +
        s"\"blocked\":${bool(dut.io.traceBlocked)}")
      // TraceBuffer registers enable && stall even with no trace entries, so
      // an empty ROB needs no filler instructions before its serialized CSR.
      until("registered trace backpressure", maximum = 16)(bool(dut.io.traceBlocked))
      val blockedCycle = cycles
      traceSourcePC = Some(pc)
      planned(pc) = request
      enqueue(Seq(request.instruction), pc)
      until("the stalled source CSR writeback")(accesses.get(pc).exists(_.writeback >= 0))
      val source = accesses(pc)
      val sourceUid = source.uid
      idle(16)
      dut.io.traceStall.expect(true.B)
      dut.io.traceBlocked.expect(true.B)
      dut.io.robHead.valid.expect(true.B)
      dut.io.robHead.writebacked.expect(true.B)
      assert(pointer(dut.io.robHead.robIdx) == source.ptr)
      assert(source.retireCount == 0 && traceBlockedSamples >= 16 && traceReadyBlockedSamples > 0)
      requireConverged(source)

      // The ordinary configuration redirect may precede this release. Only
      // retirement is required to wait for the actual trace block to clear.
      traceReleaseCycle = cycles
      dut.io.traceStall.poke(false.B)
      recordAccess("trace-stall-release", source,
        s"\"stall\":false,\"blocked\":${bool(dut.io.traceBlocked)}," +
          s"\"stall_cycle\":$stallCycle,\"blocked_cycle\":$blockedCycle," +
          s"\"release_cycle\":$traceReleaseCycle,\"rob_flush_cycle\":${source.robFlush}," +
          s"\"ftq_redirect_cycle\":${source.ftqRedirect},\"retire_count\":${source.retireCount}")
      until("trace backpressure release", maximum = 16)(!bool(dut.io.traceBlocked))
      val unblockedCycle = cycles
      val completed = finishWrite(pc)
      assert(completed.uid == sourceUid && completed.retireCount == 1)
      assert(completed.retire >= unblockedCycle && completed.recoveryAccept > completed.recoveryFetch)
      recordAccess("trace-recovery-summary", completed,
        s"\"stall_cycle\":$stallCycle,\"blocked_cycle\":$blockedCycle," +
          s"\"release_cycle\":$traceReleaseCycle,\"unblocked_cycle\":$unblockedCycle," +
          s"\"retire_cycle\":${completed.retire},\"blocked_samples\":$traceBlockedSamples," +
          s"\"ready_blocked_samples\":$traceReadyBlockedSamples," +
          s"\"recovery_fetch_cycle\":${completed.recoveryFetch},\"recovery_accept_cycle\":${completed.recoveryAccept}")
      traceSourcePC = None
    }
    def read(pc: BigInt, address: Int): Unit = {
      val request = Request(address, 0, write = false)
      planned(pc) = request
      val id = enqueue(Seq(request.instruction), pc).head
      awaitRetired(id)
      idle(16)
      val access = accesses(pc)
      assert(access.response >= 0 && access.post >= 0 && access.retireCount == 1)
      assert(access.effect < 0 && access.robFlush < 0 && access.backendRedirect < 0)
    }
    def satpControl(pc: BigInt): Unit = {
      val request = Request(0x180, 0, write = true)
      planned(pc) = request
      enqueue(Seq(request.instruction), pc)
      val access = finishWrite(pc)
      assert(!access.request.fdi && access.post < 0)
      recordAccess("satp-recovery-control", access)
    }
    def olderBeforeCSR(): Unit = {
      val pc = BigInt(0x6000)
      val request = Request(0xbc6, 24, write = true)
      planned(pc + 4) = request
      waitForwardSources(pc + 4) = pc
      val ids = enqueue(Seq(BigInt(0x00200213), request.instruction), pc)
      val access = finishWrite(pc + 4)
      assert(access.c0 > retirementCycles(allocations(ids.head)))
      assert(allocations(ids.head) != access.ptr)
    }
    def olderBranchCancelsCSR(): Unit = {
      val pc = BigInt(0x7000)
      val branch = new BranchRecovery(pc, pc + 64)
      val wrongPath = Request(0x8b3, 7, write = true)
      val beforeWords = owner.toMap
      val beforeAccesses = accesses.size
      expectedBranch = Some(branch)
      // A conditional branch can be correctly predecoded while predicted not
      // taken. Equal x0 operands resolve taken in the real branch execution unit.
      val ids = enqueue(Seq(takenBranchInstruction, wrongPath.instruction), pc)
      discarded += ids(1)
      until("the older branch redirect and retirement") {
        branch.robRedirect >= 0 && branch.ftqRedirect >= 0 && branch.retire >= 0
      }
      idle(16)
      assert(!allocations.contains(ids(1)) && !accesses.contains(pc + 4),
        "The wrong-path CSR must not acquire a ROB allocation or C0 observation")
      assert(owner.toMap == beforeWords && accesses.size == beforeAccesses)
      val target = enqueue(Seq(BigInt(0x13)), branch.target, recoveredBranch = Some(branch)).head
      awaitRetired(target)
      assert(branch.recoveryFetch > branch.ftqRedirect)
      assert(owner.toMap == beforeWords && accesses.size == beforeAccesses)
      record("branch-csr-discarded-before-allocation", s"\"branch_pc\":$pc," +
        s"\"csr_pc\":${pc + 4},\"rob_redirect\":${branch.robRedirect}," +
        s"\"ftq_redirect\":${branch.ftqRedirect},\"fetch\":${branch.recoveryFetch},\"retire\":${branch.retire}")
      expectedBranch = None
    }
    def youngerDiscard(): Unit = {
      val pc = BigInt(0x2000)
      val source = Request(0xbc5, 24, write = true)
      val younger = Request(0x8b3, 5, write = true)
      planned(pc) = source
      // The old younger CSR is deliberately absent from planned C0 requests.
      val ids = enqueue(Seq(source.instruction, BigInt(0x00100213), younger.instruction), pc)
      discarded ++= ids.tail
      until("the source FTQ redirect")(accesses.get(pc).exists(_.ftqRedirect >= 0))
      idle(16)
      assert(!allocations.contains(ids(1)) && !allocations.contains(ids(2)),
        "Younger frontend input escaped blockBackward before the configuration recovery")
      assert(!accesses.contains(pc + 8) && owner(0x8b3) == 0)
      record("younger-input-discarded", s"\"ordinary_pc\":${pc + 4},\"csr_pc\":${pc + 8}")
      val access = finishWrite(pc, recoveryInstruction = BigInt(0x00100213))
      val ordinaryRefetched = Fetch(access.recoveryPointer.get, 0)
      assert(ordinaryRefetched != ids(1) && allocations.get(ordinaryRefetched).exists(retiredPointers),
        "The discarded ordinary instruction must retire only under its recovered fetch identity")
      planned(pc + 8) = younger
      val refetched = enqueue(Seq(younger.instruction), pc + 8).head
      finishWrite(pc + 8)
      assert(allocations.contains(refetched) && owner(0x8b3) == 5)
    }
  }

  it should "retain source identity and recover through the production ROB and FTQ after each software write" in {
    val root = Paths.get(sys.props("c07.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated C07 evidence directory")
    val path = root.resolve("backend-ftq")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled = true)
    val options = p(xiangshan.DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDICSRRecoveryBackendHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())(
      "verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      val driver = new Driver(elaborated.wrapped)
      try {
        driver.reset()
        if (scenario == "minimal") {
          driver.write(0x1000, 0xbc4, 1)
        } else if (scenario == "trace") {
          driver.traceBackpressure()
        } else {
          driver.write(0x1000, 0xbc5, 16)
          driver.youngerDiscard()
          driver.write(0x3000, 0xbc4, 1)
          driver.write(0x3100, 0xbc4, 1)
          driver.write(0x3200, 0x9e1, 2, fetchStall = 7)
          driver.read(0x3300, 0xbc4)
          driver.satpControl(0x3400)
          driver.read(0x3500, 0xbc4)
          driver.olderBeforeCSR()
          driver.olderBranchCancelsCSR()
          Seq(0xbc6 -> 16, 0x9e2 -> 24, 0x9e3 -> 8, 0x890 -> 16,
            0x880 -> 3, 0x8c0 -> 24, 0x8c8 -> 1, 0x8b0 -> 17,
            0x8b1 -> 19, 0x8b2 -> 21, 0x8b3 -> 5).zipWithIndex.foreach {
            case ((address, value), index) => driver.write(BigInt(0x4000 + index * 0x100), address, value)
          }
        }
        driver.idle(24)
        assert(driver.accesses.values.forall(_.retireCount == 1))
        println(s"FDI backend recovery PASS scenario=$scenario cycles=${driver.cycles} accesses=${driver.accesses.size} " +
          s"fdiAccesses=${driver.accesses.values.count(_.request.fdi)} " +
          "backend=production ftq=production controlMirror=production remoteMirrors=transport-fixtures")
      } finally {
        Files.write(path.resolve("recovery-events.jsonl"),
          driver.events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
