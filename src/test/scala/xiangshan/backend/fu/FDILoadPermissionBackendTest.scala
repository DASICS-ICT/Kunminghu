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
import xiangshan.frontend.{BranchPredictionBundle, FtqPtr}

class FDILoadPermissionBackendTest extends AnyFlatSpec {
  behavior of "Load permission with real scheduling, data bypass and precise trap completion"
  private val enabled = sys.env.getOrElse("L01_FDI_ENABLED",
    throw new IllegalArgumentException("L01_FDI_ENABLED must be explicit")).toBoolean
  private val scenario = sys.env.getOrElse("L01_SCENARIO", "minimal")
  require(Set("minimal", "full", "remaining").contains(scenario))
  private val allowedData = BigInt("123456789abcdef0", 16)
  private val deniedData = BigInt("d35ec0de87654321", 16)
  private val followingData = BigInt("596a7b8c9daebfc0", 16)
  private val trapVector = BigInt(0x5000)
  private val followingPc = if (enabled) trapVector else BigInt(0x4010)
  private val loadAddresses = Map(BigInt(0x4000) -> BigInt(0x8000), BigInt(0x4008) -> BigInt(0x8010),
    followingPc -> BigInt(0x8020))
  private val memoryData = Map(BigInt(0x8000) -> allowedData, BigInt(0x8010) -> deniedData,
    BigInt(0x8020) -> followingData)
  private case class Pointer(flag: Boolean, value: BigInt)
  private case class Key(rob: Pointer, destination: BigInt)
  private case class Cache(lane: Int, accepted: Int, address: BigInt, id: BigInt)

  private def zero(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(zero)
    case value: Record => value.elements.values.foreach(zero)
    case other => throw new IllegalArgumentException(s"Unsupported input ${other.getClass}")
  }
  private def uint(value: UInt): BigInt = value.peek().litValue
  private def bool(value: Bool): Boolean = value.peek().litToBoolean
  private def addi(rd: Int, rs: Int, immediate: Int): BigInt =
    (BigInt(immediate & 0xfff) << 20) | (BigInt(rs) << 15) | (BigInt(rd) << 7) | 0x13
  private def csrWrite(address: Int, rs: Int): BigInt =
    (BigInt(address) << 20) | (BigInt(rs) << 15) | (BigInt(1) << 12) | 0x73
  private def load(rd: Int, offset: Int): BigInt =
    (BigInt(offset) << 20) | (BigInt(1) << 15) | (BigInt(3) << 12) | (BigInt(rd) << 7) | 3

  private class Driver(val dut: FDILoadPermissionBackendHarness) {
    var cycle = 0
    var csrResponses = 0
    var redirects = 0
    var traps = 0
    var trapCycle = -1
    var trapRecoveryCycle = -1
    var followingIssueCycle = -1
    var followingWb = 0
    var followingRetire = 0
    var deniedWb = 0
    var allowedWb = 0
    var allowedDependent = 0
    var deniedDependent = 0
    var ld1 = 0
    var ld2 = 0
    val events = mutable.ArrayBuffer.empty[String]
    val fetches = mutable.Map.empty[Pointer, BigInt]
    val renamed = mutable.Map.empty[BigInt, Key]
    val pending = mutable.ArrayBuffer.empty[Cache]
    val loadKeys = mutable.Map.empty[Key, BigInt]
    val sourceLoadKeys = mutable.Map.empty[BigInt, Key]
    val completed = mutable.Set.empty[(BigInt, Key)]
    def pointer(value: xiangshan.backend.rob.RobPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    def ftqPointer(value: FtqPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    def setPointer(value: FtqPtr, source: Pointer): Unit = {
      value.flag.poke(source.flag.B); value.value.poke(source.value.U)
    }
    def event(kind: String, detail: String): Unit =
      events += s"{\"cycle\":$cycle,\"event\":\"$kind\",$detail}"
    def edge(): Unit = {
      dut.memory.response.foreach { response => zero(response.bits); response.valid.poke(false.B) }
      for (item <- pending if item.accepted + 2 == cycle) {
        val data = memoryData(item.address)
        val response = dut.memory.response(item.lane)
        response.valid.poke(true.B)
        response.bits.id.poke(item.id.U)
        response.bits.data.poke((data | (data << 64)).U)
      }
      for (item <- pending if item.accepted + 3 == cycle) {
        val data = memoryData(item.address)
        dut.memory.response(item.lane).bits.data_delayed.poke((data | (data << 64)).U)
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady) && !bool(dut.io.ifuRedirect.valid))
        fetches(ftqPointer(dut.io.fetchRequest.bits.ftqIdx)) = uint(dut.io.fetchRequest.bits.startAddr)
      if (bool(dut.io.csrResponse.valid)) csrResponses += 1
      if (bool(dut.io.backendRedirect.valid)) redirects += 1
      if (bool(dut.io.mEntry)) { traps += 1; trapCycle = cycle }
      if (bool(dut.io.ifuRedirect.valid) && trapCycle >= 0) {
        assert(enabled && trapRecoveryCycle < 0, "Unexpected additional post-trap FTQ redirect")
        dut.io.ifuRedirect.bits.cfiUpdate.target.expect(trapVector.U)
        trapRecoveryCycle = cycle
        event("trap-ftq-recovery", s"\"target\":$trapVector,\"trap_cycle\":$trapCycle")
      }
      dut.memory.rename.foreach { entry =>
        if (bool(entry.valid)) {
          val pc = uint(entry.bits.pc)
          if (pc >= 0x4000 && pc < 0x4010 || pc == followingPc) {
            assert(!renamed.contains(pc), "Unexpected duplicate rename in the minimum")
            renamed(pc) = Key(pointer(entry.bits.robIdx), uint(entry.bits.pdest))
            event("rename", s"\"pc\":$pc,\"rob\":${uint(entry.bits.robIdx.value)},\"pdest\":${uint(entry.bits.pdest)}")
          }
        }
      }
      dut.memory.issue.zipWithIndex.foreach { case (entry, lane) =>
        if (bool(entry.valid)) {
          val pc = uint(entry.bits.uop.pc)
          assert(loadAddresses.contains(pc), s"Unexpected load source PC $pc")
          val address = loadAddresses(pc)
          val key = Key(pointer(entry.bits.uop.robIdx), uint(entry.bits.uop.pdest))
          assert(renamed(pc) == key)
          assert((uint(entry.bits.src(0)) + uint(entry.bits.uop.imm)) == address)
          assert(!sourceLoadKeys.contains(pc), "Unexpected load reissue in cache-hit minimum")
          // A real trap may recycle ROB/preg numbers. A fresh program source owns
          // the recycled key only after the prior source completed and recovered.
          loadKeys.get(key).foreach { priorPc =>
            assert(pc == followingPc && completed((priorPc, key)) && (!enabled || trapRecoveryCycle >= 0))
          }
          sourceLoadKeys(pc) = key
          loadKeys(key) = pc
          if (pc == followingPc) {
            assert(followingIssueCycle < 0 && (!enabled || cycle > trapRecoveryCycle && trapRecoveryCycle >= trapCycle))
            dut.io.mode.expect((if (enabled) 3 else 0).U)
            followingIssueCycle = cycle
          }
          event("load-issue", s"\"pc\":$pc,\"lane\":$lane,\"address\":$address")
        }
        if (bool(dut.memory.requests(lane).valid)) {
          val request = dut.memory.requests(lane).bits
          val address = uint(request.vaddr)
          assert(memoryData.contains(address))
          pending += Cache(lane, cycle, address, uint(request.id))
        }
        if (bool(dut.memory.noData(lane))) {
          dut.memory.ld1Cancel(lane).expect(true.B)
          ld1 += 1
        }
        if (bool(dut.memory.ld2Cancel(lane))) ld2 += 1
        dut.memory.splitRequest(lane).expect(false.B)
        val out = dut.memory.completion(lane)
        if (bool(out.valid)) {
          val key = Key(pointer(out.bits.uop.robIdx), uint(out.bits.uop.pdest))
          val pc = loadKeys(key)
          assert(completed.add((pc, key)), "Duplicate load completion")
          val denied = enabled && pc == 0x4008
          out.bits.uop.rfWen.expect((!denied).B)
          out.bits.uop.fpWen.expect(false.B)
          out.bits.data.expect((if (denied) BigInt(0) else memoryData(loadAddresses(pc))).U)
          if (denied) {
            deniedWb += 1
            out.bits.uop.exceptionVec(24).expect(true.B)
            out.bits.uop.fdiException.get.tval.expect(0x8010.U)
            out.bits.uop.fdiException.get.reason.expect(2.U)
            val sameDestination = dut.memory.bypass.filter(port => uint(port.destination) == key.destination)
            assert(sameDestination.nonEmpty)
            sameDestination.foreach { port => port.valid.expect(false.B); port.data.expect(0.U) }
            val faultWb = dut.memory.wb.filter(port => bool(port.fire) && pointer(port.rob) == key.rob)
            assert(faultWb.size == 1, "Denied load lost its precise completion branch")
            faultWb.head.intWen.expect(false.B); faultWb.head.fpWen.expect(false.B)
            faultWb.head.exceptions(24).expect(true.B)
          } else if (pc == followingPc) {
            followingWb += 1
            assert(followingWb == 1 && followingIssueCycle >= 0)
            out.bits.uop.exceptionVec.foreach(_.expect(false.B))
            val actualWb = dut.memory.wb.filter(port => bool(port.fire) && pointer(port.rob) == key.rob)
            assert(actualWb.size == 1, "Following load lost its real writeback branch")
            actualWb.head.intWen.expect(true.B)
            actualWb.head.fpWen.expect(false.B)
            actualWb.head.exceptions.foreach(_.expect(false.B))
          } else allowedWb += 1
          event("load-completion", s"\"pc\":$pc,\"denied\":$denied,\"data\":${uint(out.bits.data)}")
        }
      }
      dut.memory.uncacheRequest.expect(false.B)
      dut.memory.execution.foreach { execution =>
        if (bool(execution.valid)) {
          val key = Key(pointer(execution.bits.rob), uint(execution.bits.destination))
          if (renamed.get(BigInt(0x4004)).contains(key)) {
            allowedDependent += 1
            execution.bits.operand.expect(allowedData.U)
          }
          if (renamed.get(BigInt(0x400c)).contains(key)) {
            deniedDependent += 1
            assert(!enabled, "A denied-load dependent reached an actual function unit")
          }
        }
      }
      dut.io.robRetire.foreach { retirement =>
        if (bool(retirement.valid) && renamed.get(followingPc).exists(_.rob == pointer(retirement.bits))) {
          assert(followingWb == 1, "Following source retired before its actual load completion")
          followingRetire += 1
          assert(followingRetire == 1, "Following source retired twice")
          event("following-retire", s"\"pc\":$followingPc,\"rob\":${uint(retirement.bits.value)}," +
            s"\"rob_flag\":${bool(retirement.bits.flag)}")
        }
      }
      dut.clock.step(); cycle += 1
    }
    def until(label: String, limit: Int = 2000)(condition: => Boolean): Unit = {
      val stop = cycle + limit
      while (!condition && cycle < stop) edge()
      assert(condition, s"Timed out: $label at $cycle")
    }
    def idle(count: Int): Unit = for (_ <- 0 until count) edge()
    private def prediction(stage: BranchPredictionBundle, pc: BigInt, ptr: Pointer): Unit = {
      zero(stage); stage.pc.foreach(_.poke(pc.U)); stage.valid.foreach(_.poke(true.B)); setPointer(stage.ftq_idx, ptr)
    }
    def packet(pc: BigInt, instructions: Seq[BigInt], tag: Boolean = false): Unit = {
      val lanes = Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq
      require(instructions.nonEmpty && instructions.size <= lanes.size)
      val ptr = ftqPointer(dut.io.ftqNext)
      fetches -= ptr
      zero(dut.io.bpu.resp.bits)
      prediction(dut.io.bpu.resp.bits.s1, pc, ptr)
      dut.io.bpu.resp.valid.poke(true.B)
      until("BPU ready")(bool(dut.io.bpu.resp.ready)); edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1); prediction(dut.io.bpu.resp.bits.s2, pc, ptr); edge()
      zero(dut.io.bpu.resp.bits.s2); prediction(dut.io.bpu.resp.bits.s3, pc, ptr); edge()
      zero(dut.io.bpu.resp.bits.s3)
      until("actual FTQ fetch")(fetches.contains(ptr)); assert(fetches(ptr) == pc)
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
            lane.bits.instr.poke(instructions(index).U); lane.bits.pc.poke((pc + 4 * index).U)
            lane.bits.pd.valid.poke(true.B); lane.bits.trigger.poke(xiangshan.TriggerAction.None)
            lane.bits.fdiNotTrusted.foreach(_.poke(tag.B)); setPointer(lane.bits.ftqPtr, ptr)
            lane.bits.ftqOffset.poke((2 * index).U)
            lane.bits.isLastInFtqEntry.poke((index == instructions.size - 1).B)
          }
        }
        val count = (0 until instructions.size - consumed).takeWhile(index => bool(lanes(index).ready)).size
        edge(); consumed += count
      }
      assert(consumed == instructions.size)
      lanes.foreach(_.valid.poke(false.B))
    }
    def reset(): Unit = {
      (Seq(dut.io.instruction) ++ dut.io.instructionTail).foreach { lane => zero(lane.bits); lane.valid.poke(false.B) }
      zero(dut.io.bpu.resp.bits); dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.predecode); zero(dut.io.interrupts)
      dut.io.fetchReady.poke(true.B); dut.io.traceEnable.poke(false.B); dut.io.traceStall.poke(false.B)
      dut.memory.cacheReady.foreach(_.poke(true.B)); zero(dut.memory.pmp)
      dut.memory.response.foreach { response => zero(response.bits); response.valid.poke(false.B) }
      dut.memory.ptw.req.foreach(_.ready.poke(true.B)); zero(dut.memory.ptw.resp.bits)
      dut.memory.ptw.resp.valid.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(10)
      dut.reset.poke(false.B); dut.clock.step(400); cycle = 410
      dut.io.mode.expect(3.U)
    }
    def csr(pc: BigInt, instruction: BigInt, flush: Boolean): Unit = {
      val before = csrResponses
      val previousRedirects = redirects
      packet(pc, Seq(instruction))
      until("CSR response")(csrResponses > before)
      if (flush) until("CSR recovery")(redirects > previousRedirects)
      idle(16)
    }
    def minimum(): Unit = {
      packet(0x1000, Seq(BigInt(0x000080b7), addi(2, 1, 8), addi(3, 0, 10), BigInt(0x00004237)))
      idle(40)
      packet(0x1080, Seq(BigInt(0x000052b7)))
      idle(20)
      csr(0x10c0, csrWrite(0x305, 5), flush = false)
      if (enabled) {
        csr(0x1100, csrWrite(0x890, 1), flush = true)
        csr(0x1200, csrWrite(0x891, 2), flush = true)
        csr(0x1300, csrWrite(0x880, 3), flush = true)
        csr(0x1400, (BigInt(0xbc4) << 20) | (BigInt(2) << 15) | (BigInt(5) << 12) | 0x73, flush = true)
        dut.memory.mirror(0).expect(2.U)
        dut.memory.mirror(1).expect(10.U)
        dut.memory.mirror(2).expect(0x8000.U)
        dut.memory.mirror(3).expect(0x8008.U)
      }
      csr(0x1500, csrWrite(0x341, 4), flush = false)
      csr(0x1580, csrWrite(0x300, 0), flush = false)
      packet(0x1600, Seq(BigInt(0x30200073)))
      until("architectural HU entry")(uint(dut.io.mode) == 0)
      idle(20)
      packet(0x4000, Seq(load(10, 0), addi(11, 10, 1), load(12, 16), addi(13, 12, 1)), tag = true)
      if (enabled) {
        until("precise denied load trap")(traps == 1)
        idle(12)
        dut.io.mcause.expect(24.U); dut.io.mepc.expect(0x4008.U); dut.io.mtval.expect(0x8010.U)
        dut.memory.reason.expect(2.U)
        assert(deniedWb == 1 && allowedWb == 1 && allowedDependent == 1 && deniedDependent == 0)
        assert(ld1 > 0 && ld2 > 0)
      } else {
        until("disabled ordinary load consumers")(allowedWb == 2 && allowedDependent == 1 && deniedDependent == 1)
        assert(traps == 0 && deniedWb == 0)
      }
      until("real load queue drain")(bool(dut.memory.queueEmpty))
      idle(20)
      assert(completed.size == 2 && pending.size == 2)
      if (enabled) {
        until("actual trap to FTQ recovery")(trapRecoveryCycle >= 0)
        assert(trapRecoveryCycle >= trapCycle)
      }
      // This independent source enters only after the old fault has recovered.
      // Its M-mode handler access is allowed; the off control continues in HU.
      packet(followingPc, Seq(load(14, 32)), tag = true)
      until("fresh following load writeback and retirement")(followingWb == 1 && followingRetire == 1)
      until("real queue drains following load")(bool(dut.memory.queueEmpty))
      idle(20)
      assert(completed.size == 3 && pending.size == 3 && sourceLoadKeys.size == 3)
      assert(deniedDependent == (if (enabled) 0 else 1))
      assert(traps == (if (enabled) 1 else 0))
      if (enabled) dut.memory.reason.expect(2.U)
    }
  }

  private case class ConsumerKey(rob: Pointer, destination: BigInt, floating: Boolean)
  private class ConsumerDriver(module: FDILoadPermissionBackendHarness, floatingParent: Boolean,
    denyParent: Boolean) extends Driver(module) {
    private val label = s"consumer-${if (floatingParent) "fp" else "int"}-${if (denyParent) "deny" else "allow"}"
    private val parentPc = BigInt(0x4004)
    private val parentAddress = BigInt(if (denyParent) 0x8080 else 0x8000)
    private val pointerData = BigInt(0x8040)
    private val independentData = BigInt("1029384756abcdef", 16)
    private val childData = BigInt("6758493a2b1c0dfe", 16)
    private val denied = enabled && denyParent
    private val programLoads = Map(BigInt(0x4000) -> (BigInt(0x8008), independentData, false),
      parentPc -> (parentAddress, pointerData, floatingParent),
      BigInt(0x4010) -> (BigInt(0x8040), childData, false))
    private val programDestinations = Map(BigInt(0x4000) -> false, parentPc -> floatingParent,
      BigInt(0x4008) -> false, BigInt(0x400c) -> true, BigInt(0x4010) -> false)
    private val expectedRegisters = Map(BigInt(0x4000) -> independentData, parentPc -> pointerData,
      BigInt(0x4008) -> (if (floatingParent) pointerData else pointerData + 1),
      BigInt(0x400c) -> (if (floatingParent) pointerData * 2 else pointerData), BigInt(0x4010) -> childData)
    private val identities = mutable.Map.empty[BigInt, ConsumerKey]
    private val issues = mutable.Set.empty[BigInt]
    private val done = mutable.Set.empty[BigInt]
    private val consumers = mutable.Set.empty[BigInt]
    private val registerWrites = mutable.Set.empty[BigInt]
    private val retired = mutable.Set.empty[BigInt]
    private var rcPrevious = Vector.fill[Option[ConsumerKey]](dut.memory.rcWrites.size)(None)
    private val rcWritten = mutable.Set.empty[BigInt]
    private val phaseCounts = mutable.Map.empty[String, Int].withDefaultValue(0)
    private val enqueueWitnesses = mutable.Set.empty[BigInt]
    private val directConsumers = Set(BigInt(0x4008), BigInt(0x400c))
    private val allDependents = directConsumers + BigInt(0x4010)
    private val integerDependency = mutable.Map.empty[String, Set[BigInt]].withDefaultValue(Set.empty)
    private val integerCanceled = mutable.Set.empty[BigInt]
    private val fpWaiting = mutable.Set.empty[BigInt]
    private val fpReady = mutable.Set.empty[BigInt]
    private val fpDequeued = mutable.Set.empty[BigInt]
    private val fpReadCycles = mutable.Map.empty[BigInt, Int]
    private val fpWakeCycles = mutable.Map.empty[Int, Int]
    private var fpParentWriteCycle = -1
    private var loadStart = mutable.Map.empty[BigInt, (Int, Int)]
    private var simultaneousLanes = false
    private def ckey(rob: xiangshan.backend.rob.RobPtr, dest: UInt, fp: Boolean): ConsumerKey =
      ConsumerKey(pointer(rob), uint(dest), fp)
    private def owner(key: ConsumerKey): Option[BigInt] = identities.collectFirst { case (pc, found) if found == key => pc }
    private def isRejected(pc: BigInt): Boolean = denied && pc >= parentPc
    private def loadInstruction(rd: Int, rs: Int, offset: Int, fp: Boolean = false): BigInt =
      (BigInt(offset) << 20) | (BigInt(rs) << 15) | (BigInt(3) << 12) | (BigInt(rd) << 7) | (if (fp) 7 else 3)

    override def edge(): Unit = {
      dut.memory.response.foreach { response => zero(response.bits); response.valid.poke(false.B) }
      def dataAt(address: BigInt): BigInt =
        if (address == 0x8008) independentData else if (address == 0x8040) childData else pointerData
      for (item <- pending if item.accepted + 2 == cycle) {
        val response = dut.memory.response(item.lane)
        response.valid.poke(true.B); response.bits.id.poke(item.id.U)
        val data = dataAt(item.address); response.bits.data.poke((data | (data << 64)).U)
      }
      for (item <- pending if item.accepted + 3 == cycle) {
        val data = dataAt(item.address)
        dut.memory.response(item.lane).bits.data_delayed.poke((data | (data << 64)).U)
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady) && !bool(dut.io.ifuRedirect.valid))
        fetches(ftqPointer(dut.io.fetchRequest.bits.ftqIdx)) = uint(dut.io.fetchRequest.bits.startAddr)
      if (bool(dut.io.csrResponse.valid)) csrResponses += 1
      if (bool(dut.io.backendRedirect.valid)) redirects += 1
      if (bool(dut.io.mEntry)) { traps += 1; trapCycle = cycle }
      dut.memory.rename.foreach { entry =>
        if (bool(entry.valid) && programDestinations.contains(uint(entry.bits.pc))) {
          val pc = uint(entry.bits.pc)
          assert(!identities.contains(pc))
          val fp = programDestinations(pc)
          entry.bits.fpWen.expect(fp.B); entry.bits.rfWen.expect((!fp).B)
          identities(pc) = ckey(entry.bits.robIdx, entry.bits.pdest, fp)
          event("consumer-rename", s"\"case\":\"$label\",\"pc\":$pc,\"pdest\":${uint(entry.bits.pdest)},\"fp\":$fp")
        }
      }
      if (floatingParent) identities.get(parentPc).foreach { parent =>
        dut.memory.registerWrites.foreach { write =>
          if (bool(write.valid) && bool(write.floating) && uint(write.destination) == parent.destination) {
            assert(!denied, "Denied FP parent produced an actual FP write")
            write.data.expect(pointerData.U)
            assert(fpParentWriteCycle < 0 || fpParentWriteCycle == cycle)
            fpParentWriteCycle = cycle
          }
        }
        dut.memory.fpWbWakeup.foreach { wakeup =>
          if (bool(wakeup.valid) && bool(wakeup.floating) && uint(wakeup.destination) == parent.destination) {
            assert(!denied, "Denied FP parent produced a matching FPIQ writeback wakeup")
            assert(fpParentWriteCycle == cycle, "FPIQ wakeup did not match the actual parent FP write")
            val queue = uint(wakeup.queue).toInt
            assert(!fpWakeCycles.contains(queue) || fpWakeCycles(queue) == cycle)
            fpWakeCycles(queue) = cycle
            event("fp-parent-writeback-wakeup", s"\"case\":\"$label\",\"parent_pc\":$parentPc," +
              s"\"parent_rob\":${parent.rob.value},\"parent_flag\":${parent.rob.flag}," +
              s"\"fp_pdest\":${parent.destination},\"queue\":$queue")
          }
        }
      }
      for ((ports, phase) <- Seq(dut.memory.iqEntries -> "iq-entry", dut.memory.iqEnqueue -> "iq-enqueue",
        dut.memory.iqDequeue -> "iq-dequeue", dut.memory.iqDelayed -> "iq-delayed",
        dut.memory.dataInput -> "datapath-input", dut.memory.dataOutput -> "datapath-output")) {
        ports.zipWithIndex.foreach { case (port, index) =>
          if (bool(port.valid)) owner(ckey(port.rob, port.destination, bool(port.floating))).foreach { pc =>
            val dependency = port.dependency.map(uint)
            val active = dependency.exists(_ != 0)
            if (active) phaseCounts(phase) += 1
            if (phase == "iq-enqueue" && bool(port.ready)) enqueueWitnesses += pc
            if (!floatingParent && allDependents(pc) && active &&
              Set("iq-entry", "iq-dequeue", "datapath-input").contains(phase)) {
              val parent = identities(parentPc)
              val lane = loadStart(parentPc)._2
              port.sourceRegister(0).expect(parent.destination.U)
              port.sourceFloating(0).expect(false.B)
              assert(dependency(lane) != 0, "Dependency belonged to another load lane")
              integerDependency(phase) = integerDependency(phase) + pc
              if (denied && phase == "datapath-input" && bool(port.ready)) {
                val cancel = dependency(lane).testBit(0) && bool(dut.memory.ld1Cancel(lane)) ||
                  dependency(lane).testBit(1) && bool(dut.memory.ld2Cancel(lane))
                assert(cancel, "Denied parent's dependent escaped its actual dependency-age cancel")
                integerCanceled += pc
                event("integer-parent-cancel-match", s"\"case\":\"$label\",\"parent_pc\":$parentPc," +
                  s"\"consumer_pc\":$pc,\"lane\":$lane,\"dependency\":${dependency(lane)}")
              }
            }
            if (floatingParent && directConsumers(pc) &&
              Set("iq-entry", "iq-enqueue", "iq-dequeue", "datapath-input").contains(phase)) {
              val parent = identities(parentPc)
              val sources = if (pc == 0x400c) Seq(0, 1) else Seq(0)
              sources.foreach { index =>
                port.sourceUsed(index).expect(true.B)
                port.sourceFloating(index).expect(true.B)
                port.sourceRegister(index).expect(parent.destination.U)
              }
              val queue = uint(port.queue).toInt
              if (phase == "iq-entry" || phase == "iq-enqueue") {
                for (index <- 0 until 3 if bool(port.sourceUsed(index)) && !sources.contains(index))
                  port.sourceReady(index).expect(true.B)
                if (sources.forall(index => !bool(port.sourceReady(index)))) fpWaiting += pc
                if (sources.exists(index => bool(port.sourceReady(index)))) {
                  assert(!denied, "Denied FP parent made its queued source ready")
                  assert(sources.forall(index => bool(port.sourceReady(index))))
                  assert(fpParentWriteCycle >= 0 && cycle >= fpParentWriteCycle &&
                    fpWakeCycles.get(queue).exists(_ <= cycle), "FP source became ready without its parent WB wakeup")
                  fpReady += pc
                }
              }
              if (phase == "iq-dequeue" && bool(port.ready)) {
                assert(!denied && fpWaiting(pc) && fpReady(pc) && fpWakeCycles.get(queue).exists(_ <= cycle),
                  "FP consumer dequeued without its observed parent writeback path")
                fpDequeued += pc
              }
              if (phase == "datapath-input" && bool(port.ready)) {
                assert(!denied && fpDequeued(pc) && cycle > fpParentWriteCycle)
                sources.foreach(index => port.registerRead(index).expect(true.B))
                fpReadCycles(pc) = cycle
                event("fp-parent-register-read", s"\"case\":\"$label\",\"consumer_pc\":$pc," +
                  s"\"fp_pdest\":${parent.destination},\"parent_write_cycle\":$fpParentWriteCycle")
              }
            }
            event(phase, s"\"case\":\"$label\",\"pc\":$pc,\"port\":$index,\"ready\":${bool(port.ready)}," +
              s"\"entry_class\":${uint(port.entryClass)},\"queue\":${uint(port.queue)}," +
              s"\"source_register\":[${port.sourceRegister.map(uint).mkString(",")}]," +
              s"\"source_floating\":[${port.sourceFloating.map(bool).mkString(",")}]," +
              s"\"source_ready\":[${port.sourceReady.map(bool).mkString(",")}],\"dependency\":[${dependency.mkString(",")}]," +
              s"\"ld1\":[${dut.memory.ld1Cancel.map(bool).mkString(",")}],\"ld2\":[${dut.memory.ld2Cancel.map(bool).mkString(",")}]")
          }
        }
      }
      dut.memory.iqWakeup.zipWithIndex.foreach { case (port, index) =>
        if (bool(port.valid) && identities.values.exists(_.destination == uint(port.destination)))
          event("iq-wakeup", s"\"case\":\"$label\",\"port\":$index,\"pdest\":${uint(port.destination)}," +
            s"\"integer\":${bool(port.integer)},\"floating\":${bool(port.floating)}," +
            s"\"dependency\":[${port.dependency.map(uint).mkString(",")}]")
      }
      dut.memory.issue.zipWithIndex.foreach { case (entry, lane) =>
        if (bool(entry.valid)) {
          val pc = uint(entry.bits.uop.pc)
          assert(programLoads.contains(pc), s"$label: unexpected load source $pc")
          assert(!isRejected(pc) || pc == parentPc, "A denied load became a dependent memory address")
          val (address, _, fp) = programLoads(pc)
          assert(identities(pc) == ckey(entry.bits.uop.robIdx, entry.bits.uop.pdest, fp))
          assert(issues.add(pc))
          assert(uint(entry.bits.src(0)) + uint(entry.bits.uop.imm) == address)
          loadStart(pc) = (cycle, lane)
          event("consumer-load-issue", s"\"case\":\"$label\",\"pc\":$pc,\"lane\":$lane,\"address\":$address")
        }
        if (bool(dut.memory.requests(lane).valid)) {
          val req = dut.memory.requests(lane).bits
          val address = uint(req.vaddr)
          assert(programLoads.values.exists(_._1 == address))
          pending += Cache(lane, cycle, address, uint(req.id))
        }
        if (bool(dut.memory.wakeup(lane)))
          event("load-predicted-wakeup", s"\"case\":\"$label\",\"lane\":$lane")
        if (bool(dut.memory.noData(lane))) { dut.memory.ld1Cancel(lane).expect(true.B); ld1 += 1 }
        if (bool(dut.memory.ld2Cancel(lane))) ld2 += 1
        val out = dut.memory.completion(lane)
        if (bool(out.valid)) {
          val key = ckey(out.bits.uop.robIdx, out.bits.uop.pdest,
            programLoads(uint(out.bits.uop.pc))._3)
          val pc = owner(key).get
          val (_, data, fp) = programLoads(pc)
          assert(done.add(pc))
          val rejected = isRejected(pc)
          out.bits.uop.rfWen.expect((!fp && !rejected).B)
          out.bits.uop.fpWen.expect((fp && !rejected).B)
          out.bits.data.expect((if (rejected) BigInt(0) else data).U)
          out.bits.uop.exceptionVec.zipWithIndex.foreach { case (bit, cause) => bit.expect((rejected && cause == 24).B) }
          if (rejected) {
            out.bits.uop.fdiException.get.tval.expect(parentAddress.U)
            out.bits.uop.fdiException.get.reason.expect(2.U)
            val bypass = dut.memory.bypass.filter(port => uint(port.destination) == key.destination)
            assert(bypass.nonEmpty); bypass.foreach { port => port.valid.expect(false.B); port.data.expect(0.U) }
          }
          val wb = dut.memory.wb.filter(port => bool(port.fire) && pointer(port.rob) == key.rob)
          assert(wb.size == 1)
          wb.head.intWen.expect((!fp && !rejected).B); wb.head.fpWen.expect((fp && !rejected).B)
          event("consumer-load-completion", s"\"case\":\"$label\",\"pc\":$pc,\"denied\":$rejected")
        }
      }
      if (loadStart.contains(0x4000) && loadStart.contains(parentPc)) {
        val a = loadStart(0x4000); val b = loadStart(parentPc)
        if (a._1 == b._1 && a._2 != b._2) simultaneousLanes = true
      }
      dut.memory.execution.foreach { execution =>
        if (bool(execution.valid)) owner(ckey(execution.bits.rob, execution.bits.destination,
          bool(execution.bits.floating))).foreach { pc =>
          if (pc == 0x4008 || pc == 0x400c) {
            assert(!isRejected(pc), s"$label: rejected operand reached a real integer/FP FU")
            if (floatingParent) assert(fpReadCycles.get(pc).exists(_ < cycle),
              "FP operand reached the FU before its observed register-read path")
            execution.bits.operand.expect(pointerData.U)
            assert(consumers.add(pc), "Unexpected repeated consumer execution")
            event("consumer-fu", s"\"case\":\"$label\",\"pc\":$pc,\"operand\":$pointerData")
          }
        }
      }
      dut.memory.registerWrites.foreach { port =>
        if (bool(port.valid)) identities.find { case (_, key) =>
          key.destination == uint(port.destination) && key.floating == bool(port.floating)
        }.foreach { case (pc, _) =>
          assert(!isRejected(pc), s"$label: denied source wrote an integer/FP physical register")
          port.data.expect(expectedRegisters(pc).U)
          assert(registerWrites.add(pc), s"$label: duplicate register write for $pc")
          event("register-write", s"\"case\":\"$label\",\"pc\":$pc,\"fp\":${bool(port.floating)}")
        }
      }
      dut.memory.rcWrites.zipWithIndex.foreach { case (port, index) =>
        if (bool(port.valid)) rcPrevious(index).flatMap(owner).foreach { pc =>
          assert(!isRejected(pc), s"$label: denied data was captured by the register cache")
          port.data.expect(expectedRegisters(pc).U)
          rcWritten += pc
          event("regcache-write", s"\"case\":\"$label\",\"pc\":$pc,\"port\":$index")
        }
      }
      rcPrevious = dut.memory.rcWrites.map { port =>
        identities.values.find(key => !key.floating && key.destination == uint(port.sourceDestination))
      }.toVector
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid)) identities.foreach { case (pc, key) =>
          if (key.rob == pointer(port.bits)) {
            assert(!isRejected(pc), s"$label: denied source or descendant retired")
            retired += pc
          }
        }
      }
      dut.memory.uncacheRequest.expect(false.B); dut.memory.splitRequest.foreach(_.expect(false.B))
      dut.clock.step(); cycle += 1
    }

    def runConsumers(): Unit = {
      reset()
      event("consumer-case", s"\"case\":\"$label\",\"denied\":$denied")
      packet(0x1000, Seq(BigInt(0x000080b7), addi(2, 1, 0x50), addi(3, 0, 10), BigInt(0x00004237),
        BigInt(0x000022b7)))
      idle(40)
      if (enabled) {
        csr(0x1100, csrWrite(0x890, 1), flush = true)
        csr(0x1200, csrWrite(0x891, 2), flush = true)
        csr(0x1300, csrWrite(0x880, 3), flush = true)
        csr(0x1400, (BigInt(0xbc4) << 20) | (BigInt(2) << 15) | (BigInt(5) << 12) | 0x73, flush = true)
      }
      csr(0x1500, csrWrite(0x341, 4), flush = false)
      csr(0x1580, csrWrite(0x300, 5), flush = false)
      packet(0x1600, Seq(BigInt(0x30200073)))
      until("consumer HU entry")(uint(dut.io.mode) == 0)
      idle(20)
      val firstConsumer = if (floatingParent) (BigInt(0x71) << 25) | (BigInt(10) << 15) | (BigInt(12) << 7) | 0x53
        else addi(11, 10, 1)
      val secondConsumer = if (floatingParent) (BigInt(1) << 25) | (BigInt(10) << 20) | (BigInt(10) << 15) | (BigInt(11) << 7) | 0x53
        else (BigInt(0x79) << 25) | (BigInt(10) << 15) | (BigInt(11) << 7) | 0x53
      packet(0x4000, Seq(loadInstruction(20, 1, 8),
        loadInstruction(10, 1, if (denyParent) 128 else 0, floatingParent), firstConsumer, secondConsumer,
        loadInstruction(13, if (floatingParent) 12 else 10, 0)), tag = true)
      if (denied) {
        until("consumer denied trap")(traps == 1)
        idle(12)
        dut.io.mcause.expect(24.U); dut.io.mepc.expect(parentPc.U); dut.io.mtval.expect(parentAddress.U)
        dut.memory.reason.expect(2.U)
        assert(done == Set(BigInt(0x4000), parentPc) && consumers.isEmpty)
        assert(registerWrites == Set(BigInt(0x4000)))
      } else {
        until("all consumer register writes and retirement")(registerWrites.size == 5 && retired.size == 5)
        assert(done.size == 3 && consumers == Set(BigInt(0x4008), BigInt(0x400c)))
        assert(traps == 0)
      }
      until("consumer real LQ drain")(bool(dut.memory.queueEmpty))
      idle(20)
      assert(rcWritten.contains(0x4000), "The allowed control never exercised a real RC write")
      assert(Set(BigInt(0x4008), BigInt(0x400c), BigInt(0x4010)).subsetOf(enqueueWitnesses),
        "A proposed dependent never entered its actual issue queue")
      if (denied) assert(ld1 > 0 && ld2 > 0)
      // This profile connects LDU IQ wakeups to integer/memory queues. FP
      // consumers instead require an observed parent writeback-wakeup/read path.
      if (!floatingParent) {
        assert(phaseCounts("iq-entry") > 0 && phaseCounts("iq-dequeue") > 0 && phaseCounts("datapath-input") > 0,
          "The integer control never exercised actual load-dependency propagation")
        for (phase <- Seq("iq-entry", "iq-dequeue", "datapath-input"))
          assert(allDependents.subsetOf(integerDependency(phase)), s"Missing parent-matched dependency at $phase")
        if (denied) assert(allDependents.subsetOf(integerCanceled), "Missing actual parent-lane cancellation")
      } else {
        assert(directConsumers.subsetOf(fpWaiting), "FP consumers were not observed waiting for this parent")
        if (denied) assert(fpParentWriteCycle < 0 && fpWakeCycles.isEmpty && fpReady.isEmpty &&
          fpDequeued.isEmpty && fpReadCycles.isEmpty && consumers.isEmpty)
        else assert(fpParentWriteCycle >= 0 && directConsumers.subsetOf(fpReady) &&
          directConsumers.subsetOf(fpDequeued) && directConsumers.subsetOf(fpReadCycles.keySet) &&
          directConsumers.subsetOf(consumers), "The allowed FP control never exercised its actual writeback-wakeup-read-FU path")
      }
      event("consumer-case-complete", s"\"case\":\"$label\",\"register_writes\":${registerWrites.size}," +
        s"\"rc_sources\":${rcWritten.size},\"simultaneous_lanes\":$simultaneousLanes," +
        s"\"integer_parent_cancel_consumers\":${integerCanceled.size},\"fp_parent_write_cycle\":$fpParentWriteCycle," +
        s"\"fp_waiting_consumers\":${fpWaiting.size},\"fp_ready_consumers\":${fpReady.size}," +
        s"\"fp_dequeue_consumers\":${fpDequeued.size},\"fp_register_read_consumers\":${fpReadCycles.size}")
    }
  }

  private case class ArchitecturalLoad(pc: BigInt, address: BigInt, data: BigInt,
    privilege: Int, virtual: Boolean, outcome: Int, faults: Set[Int],
    lateError: Boolean = false, pmpFault: Boolean = false, mmio: Boolean = false) {
    def blocked: Boolean = outcome != 0
  }
  private case class ArchitecturalAttempt(lane: Int, accepted: Int, source: ArchitecturalLoad, id: BigInt)

  private class ArchitecturalDriver(module: FDILoadPermissionBackendHarness, label: String) extends Driver(module) {
    val planned = mutable.Map.empty[BigInt, ArchitecturalLoad]
    val sourceKeys = mutable.Map.empty[BigInt, Key]
    private val loadOwners = mutable.Map.empty[Key, BigInt]
    private val destinationOwners = mutable.Map.empty[BigInt, BigInt]
    private val activeRobSources = mutable.Map.empty[Pointer, Set[BigInt]]
    val seenRename = mutable.Map.empty[BigInt, Key]
    val frontendAccepted = mutable.Map.empty[BigInt, (Pointer, BigInt, Int)]
    private val robAllocatedSources = mutable.Set.empty[BigInt]
    val finished = mutable.Set.empty[BigInt]
    val retiredSources = mutable.Set.empty[BigInt]
    val retirementCounts = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    val canceledSources = mutable.Set.empty[BigInt]
    val watched = mutable.Set.empty[BigInt]
    val attempts = mutable.ArrayBuffer.empty[ArchitecturalAttempt]
    val redirectsSeen = mutable.ArrayBuffer.empty[(Int, BigInt, Pointer)]
    val distributionSeen = mutable.ArrayBuffer.empty[(Int, Int, BigInt)]
    var hsTraps = 0
    var debugTraps = 0
    var permissionRequests = 0
    var permissionResponses = 0
    var reasonEffects = 0
    var setupPc = BigInt(0x1000)
    private var rcPrevious = Vector.fill[Option[BigInt]](dut.memory.rcWrites.size)(None)
    private var expectedRead: Option[BigInt] = None
    private def owner(key: Key): Option[BigInt] = loadOwners.get(key)
    private def oldestOrEqual(candidate: Pointer, redirect: Pointer): Boolean =
      if (candidate.flag == redirect.flag) candidate.value <= redirect.value else candidate.value > redirect.value
    def instruction(word: BigInt, tag: Boolean = false): Unit = {
      packet(setupPc, Seq(word), tag); setupPc += 0x40
      idle(12)
    }
    def setRegister(reg: Int, value: BigInt): Unit = {
      def materialize(number: BigInt): Seq[BigInt] = {
        if (number >= -2048 && number <= 2047) Seq(addi(reg, 0, number.toInt))
        else {
          val lowUnsigned = (number & 0xfff).toInt
          val low = if (lowUnsigned >= 2048) lowUnsigned - 4096 else lowUnsigned
          val high = (number - low) >> 12
          materialize(high) ++ Seq((BigInt(12) << 20) | (BigInt(reg) << 15) | (BigInt(1) << 12) |
            (BigInt(reg) << 7) | 0x13) ++ (if (low == 0) Seq.empty else Seq(addi(reg, reg, low)))
        }
      }
      materialize(value).grouped(dut.DecodeWidth).foreach { words =>
        packet(setupPc, words); setupPc += 0x40; idle(16)
      }
    }
    def writeCSR(address: Int, value: BigInt, flush: Boolean): Unit = {
      setRegister(6, value)
      csr(setupPc, csrWrite(address, 6), flush); setupPc += 0x40
    }
    def prepare(privilege: Int, virtual: Boolean = false, delegate: Boolean = false,
      mstatusExtra: BigInt = 0): Unit = {
      reset()
      event("architectural-case", s"\"case\":\"$label\"")
      setRegister(1, 0x8000)
      writeCSR(0x305, 0x6000, flush = false)
      writeCSR(0x105, 0x7000, flush = false)
      writeCSR(0x300, 0, flush = false)
      if (enabled) {
        writeCSR(0x890, 0x8000, flush = true)
        writeCSR(0x891, 0x8008, flush = true)
        writeCSR(0x880, 10, flush = true)
        writeCSR(0xbc4, if (privilege == 1) 1 else 2, flush = true)
        writeCSR(0x8b3, 7, flush = true)
      }
      if (delegate) writeCSR(0x302, BigInt(1) << 25, flush = false)
      if (privilege != 3) {
        writeCSR(0x341, 0x4000, flush = false)
        writeCSR(0x300, (BigInt(privilege) << 11) | (if (virtual) BigInt(1) << 39 else BigInt(0)) | mstatusExtra,
          flush = false)
        instruction(BigInt(0x30200073))
      } else if (mstatusExtra != 0) writeCSR(0x300, mstatusExtra, flush = false)
      until("actual architectural source established")(uint(dut.io.mode) == privilege && bool(dut.io.virtualMode) == virtual)
      idle(12)
      dut.memory.sourcePrivilege.expect(privilege.U); dut.memory.sourceVirtual.expect(virtual.B)
    }
    override def edge(): Unit = {
      dut.memory.response.foreach { response => zero(response.bits); response.valid.poke(false.B) }
      zero(dut.memory.pmp)
      for (attempt <- attempts if attempt.accepted + 2 == cycle) {
        val response = dut.memory.response(attempt.lane)
        val source = attempt.source
        response.valid.poke(true.B); response.bits.id.poke(attempt.id.U)
        response.bits.data.poke((source.data | (source.data << 64)).U)
        dut.memory.pmp(attempt.lane).ld.poke(source.pmpFault.B)
        dut.memory.pmp(attempt.lane).mmio.poke(source.mmio.B)
      }
      for (attempt <- attempts if attempt.accepted + 3 == cycle) {
        val response = dut.memory.response(attempt.lane)
        response.bits.data_delayed.poke((attempt.source.data | (attempt.source.data << 64)).U)
        response.bits.error_delayed.poke(attempt.source.lateError.B)
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady) && !bool(dut.io.ifuRedirect.valid))
        fetches(ftqPointer(dut.io.fetchRequest.bits.ftqIdx)) = uint(dut.io.fetchRequest.bits.startAddr)
      if (bool(dut.io.csrResponse.valid)) {
        csrResponses += 1
        expectedRead.foreach(value => dut.io.csrResponse.bits.expect(value.U))
      }
      (Seq(dut.io.instruction) ++ dut.io.instructionTail).foreach { input =>
        if (bool(input.valid) && bool(input.ready)) {
          val pc = uint(input.bits.pc)
          if (watched(pc) || planned.contains(pc)) {
            assert(!frontendAccepted.contains(pc))
            frontendAccepted(pc) = (ftqPointer(input.bits.ftqPtr), uint(input.bits.ftqOffset), cycle)
            event("architectural-frontend-accept", s"\"case\":\"$label\",\"pc\":$pc")
          }
        }
      }
      dut.memory.rename.foreach { port =>
        if (bool(port.valid)) {
          val pc = uint(port.bits.pc)
          val rob = pointer(port.bits.robIdx)
          val previous = activeRobSources.getOrElse(rob, Set.empty)
          if (previous.forall(pc => retiredSources(pc) || canceledSources(pc))) activeRobSources(rob) = Set.empty
          if (bool(port.bits.rfWen)) destinationOwners -= uint(port.bits.pdest)
          if (watched(pc) || planned.contains(pc)) {
            assert(!seenRename.contains(pc))
            seenRename(pc) = Key(pointer(port.bits.robIdx), uint(port.bits.pdest))
            activeRobSources(rob) = activeRobSources.getOrElse(rob, Set.empty) + pc
            event("architectural-rename", s"\"case\":\"$label\",\"pc\":$pc," +
              s"\"rob\":${uint(port.bits.robIdx.value)},\"flag\":${bool(port.bits.robIdx.flag)},\"pdest\":${uint(port.bits.pdest)}")
          }
        }
      }
      // The support tap reports enqueue valid/canAccept; the ROB accepts a
      // first uop only when the same native redirect does not suppress enqueue.
      if (!bool(dut.memory.executionRedirect.valid)) dut.io.robEnq.foreach { enq =>
        if (bool(enq.valid) && bool(enq.bits.first)) {
          val rob = pointer(enq.bits.robIdx)
          activeRobSources.getOrElse(rob, Set.empty).foreach { pc =>
            if (robAllocatedSources.add(pc)) event("architectural-rob-allocate",
              s"\"case\":\"$label\",\"pc\":$pc,\"rob\":${rob.value},\"flag\":${rob.flag}")
          }
        }
      }
      if (bool(dut.io.backendRedirect.valid)) redirects += 1
      if (bool(dut.memory.executionRedirect.valid)) {
        val red = dut.memory.executionRedirect.bits
        val rob = pointer(red.robIdx)
        event("architectural-execution-redirect", s"\"case\":\"$label\",\"rob\":${rob.value}," +
          s"\"flag\":${rob.flag},\"level\":${uint(red.level)},\"ftq\":${uint(red.ftqIdx.value)}," +
          s"\"ftq_flag\":${bool(red.ftqIdx.flag)},\"offset\":${uint(red.ftqOffset)}")
        seenRename.foreach { case (pc, key) =>
          val killed = !oldestOrEqual(key.rob, rob) || key.rob == rob && uint(red.level) == 1
          if (killed && !retiredSources(pc) && canceledSources.add(pc)) {
            val stage = if (robAllocatedSources(pc)) "rob" else "rename"
            event("architectural-source-cancel", s"\"case\":\"$label\",\"pc\":$pc," +
              s"\"renamed\":true,\"owner_stage\":\"$stage\",\"rob\":${key.rob.value},\"flag\":${key.rob.flag}")
          }
        }
        val ftq = ftqPointer(red.ftqIdx)
        // Frontend-only sources have no allocated ROB identity. Renamed sources
        // are handled above and never reclassified by the frontend export level.
        frontendAccepted.filterNot { case (pc, _) => seenRename.contains(pc) }.foreach { case (pc, (pointer, offset, _)) =>
          val killed = !oldestOrEqual(pointer, ftq) || pointer == ftq &&
            (offset > uint(red.ftqOffset) || offset == uint(red.ftqOffset) && uint(red.level) == 1)
          if (killed && !retiredSources(pc) && canceledSources.add(pc)) {
            event("architectural-source-cancel", s"\"case\":\"$label\",\"pc\":$pc," +
              "\"renamed\":false,\"owner_stage\":\"frontend\"")
          }
        }
      }
      if (bool(dut.io.ifuRedirect.valid)) {
        val red = dut.io.ifuRedirect.bits
        redirectsSeen += ((cycle, uint(red.cfiUpdate.target), pointer(red.robIdx)))
        event("architectural-ftq-redirect", s"\"case\":\"$label\",\"target\":${uint(red.cfiUpdate.target)}")
      }
      if (bool(dut.memory.distribution.w.valid)) {
        val write = dut.memory.distribution.w.bits
        distributionSeen += ((cycle, uint(write.addr).toInt, uint(write.data)))
        event("architectural-distribution", s"\"case\":\"$label\",\"address\":${uint(write.addr)},\"data\":${uint(write.data)}")
      }
      if (bool(dut.io.mEntry)) { traps += 1; trapCycle = cycle }
      if (bool(dut.io.hsEntry)) { hsTraps += 1; trapCycle = cycle }
      if (bool(dut.io.debugEntry)) debugTraps += 1
      if (bool(dut.memory.reasonEffect)) reasonEffects += 1
      dut.memory.issue.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) {
          val pc = uint(port.bits.uop.pc)
          val source = planned.getOrElse(pc, throw new AssertionError(s"$label: unplanned load $pc"))
          assert(!sourceKeys.contains(pc) && !canceledSources(pc))
          val key = Key(pointer(port.bits.uop.robIdx), uint(port.bits.uop.pdest))
          assert(seenRename(pc) == key)
          sourceKeys(pc) = key
          loadOwners.get(key).foreach(prior => assert(finished(prior) || canceledSources(prior)))
          loadOwners(key) = pc
          destinationOwners(key.destination) = pc
          assert(uint(port.bits.src(0)) + uint(port.bits.uop.imm) == source.address)
          dut.memory.sourcePrivilege.expect(source.privilege.U)
          dut.memory.sourceVirtual.expect(source.virtual.B)
          event("architectural-load-issue", s"\"case\":\"$label\",\"pc\":$pc,\"lane\":$lane," +
            s"\"address\":${source.address},\"privilege\":${source.privilege},\"virtual\":${source.virtual}")
        }
        if (bool(dut.memory.requests(lane).valid)) {
          assert(bool(port.valid), "The bounded architectural cases do not fabricate replay requests")
          val source = planned(uint(port.bits.uop.pc))
          val req = dut.memory.requests(lane).bits
          req.vaddr.expect(source.address.U)
          attempts += ArchitecturalAttempt(lane, cycle, source, uint(req.id))
        }
        if (bool(dut.memory.permissionRequest(lane).valid)) {
          permissionRequests += 1
          val attempt = attempts.find(a => a.lane == lane && a.accepted + 1 == cycle).get
          val source = attempt.source
          val req = dut.memory.permissionRequest(lane).bits
          req.address.expect(source.address.U); req.sourcePrivilege.expect(source.privilege.U)
          req.sourceVirtual.expect(source.virtual.B); req.sizeLog2.expect(3.U); req.notTrusted.expect(true.B)
          event("architectural-permission", s"\"case\":\"$label\",\"pc\":${source.pc},\"address\":${source.address}," +
            s"\"privilege\":${source.privilege},\"virtual\":${source.virtual}")
        }
        if (bool(dut.memory.permissionResponse(lane).valid)) {
          permissionResponses += 1
          val source = attempts.find(a => a.lane == lane && a.accepted + 2 == cycle).get.source
          dut.memory.permissionResponse(lane).bits.outcome.expect(source.outcome.U)
          dut.memory.permissionResponse(lane).bits.request.address.expect(source.address.U)
        }
        val result = dut.memory.completion(lane)
        if (bool(result.valid)) {
          val key = Key(pointer(result.bits.uop.robIdx), uint(result.bits.uop.pdest))
          val pc = owner(key).get
          val source = planned(pc)
          assert(!canceledSources(pc) && finished.add(pc))
          result.bits.uop.exceptionVec.zipWithIndex.foreach { case (bit, index) => bit.expect(source.faults(index).B) }
          if (source.blocked) {
            result.bits.data.expect(0.U); result.bits.uop.rfWen.expect(false.B); result.bits.uop.fpWen.expect(false.B)
            val bypass = dut.memory.bypass.filter(p => uint(p.destination) == key.destination)
            assert(bypass.nonEmpty); bypass.foreach { p => p.valid.expect(false.B); p.data.expect(0.U) }
            val wb = dut.memory.wb.filter(p => bool(p.fire) && pointer(p.rob) == key.rob)
            assert(wb.size == 1); wb.head.intWen.expect(false.B); wb.head.fpWen.expect(false.B)
            if (source.outcome == 1) {
              result.bits.uop.fdiException.get.tval.expect(source.address.U)
              result.bits.uop.fdiException.get.reason.expect(2.U)
            }
          } else if (source.faults.isEmpty) {
            result.bits.data.expect(source.data.U); result.bits.uop.rfWen.expect(true.B)
          }
          event("architectural-load-completion", s"\"case\":\"$label\",\"pc\":$pc," +
            s"\"vector\":[${source.faults.toSeq.sorted.mkString(",")}],\"blocked\":${source.blocked}")
        }
      }
      dut.memory.registerWrites.foreach { write =>
        if (bool(write.valid) && !bool(write.floating)) destinationOwners.get(uint(write.destination))
          .foreach(pc => assert(!planned(pc).blocked && !canceledSources(pc), "Blocked load published to the integer register path"))
      }
      dut.memory.rcWrites.zipWithIndex.foreach { case (write, index) =>
        if (bool(write.valid)) rcPrevious(index).foreach(pc =>
          assert(!planned(pc).blocked && !canceledSources(pc), "Blocked load published to the RC path"))
      }
      rcPrevious = dut.memory.rcWrites.map { port =>
        destinationOwners.get(uint(port.sourceDestination))
      }.toVector
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid)) activeRobSources.getOrElse(pointer(port.bits), Set.empty).foreach { pc =>
            assert(robAllocatedSources(pc), "A source retired without observed ROB allocation")
            assert(!canceledSources(pc) && !planned.get(pc).exists(_.faults.nonEmpty), "Canceled or faulting source retired")
            retiredSources += pc
            retirementCounts(pc) += 1
            assert(retirementCounts(pc) == 1, "Source retired twice")
        }
      }
      dut.memory.uncacheRequest.expect(false.B); dut.memory.splitRequest.foreach(_.expect(false.B))
      dut.clock.step(); cycle += 1
    }
    def runFault(name: String): Unit = {
      val privilege = if (name == "supervisor" || name == "supervisor-delegated" || name == "guest") 1 else 0
      val virtual = name == "guest"
      val delegated = name == "supervisor-delegated" && enabled
      prepare(privilege, virtual, delegate = delegated)
      val blocked = enabled
      val outcome = if (!blocked) 0 else if (virtual) 2 else 1
      val policy = if (!blocked) Set.empty[Int] else Set(if (virtual) 2 else if (privilege == 1) 25 else 24)
      val standard = if (name == "late-error") Set(19) else if (name == "pmp-error") Set(5) else Set.empty[Int]
      val source = ArchitecturalLoad(0x4000, 0x8010, BigInt("e1d2c3b4a5968778", 16), privilege, virtual,
        outcome, policy ++ standard, lateError = name == "late-error", pmpFault = name == "pmp-error", mmio = name == "mmio")
      planned(source.pc) = source
      if (source.lateError) dut.memory.cacheErrorsEnabled.expect(true.B)
      packet(source.pc, Seq(load(10, 16)), tag = true)
      until("actual load completion")(finished(source.pc))
      if (source.faults.nonEmpty) {
        until("actual final trap owner")(traps + hsTraps == 1)
        idle(12)
        val winner = standard.headOption.getOrElse(policy.head)
        val csrCause = if (delegated) dut.io.scause else dut.io.mcause
        val csrEpc = if (delegated) dut.io.sepc else dut.io.mepc
        csrCause.expect(winner.U); csrEpc.expect(source.pc.U)
        val trapValue = if (delegated) dut.memory.supervisorTval else dut.io.mtval
        trapValue.expect((if (winner == 24 || winner == 25 || winner == 5) source.address else BigInt(0)).U)
        if (enabled) dut.memory.reason.expect((if (winner == 24 || winner == 25) 2 else 7).U)
        assert(reasonEffects == (if (enabled && (winner == 24 || winner == 25)) 1 else 0))
      } else until("ordinary off load retires")(retiredSources(source.pc))
      until("fault case actual LQ cleanup")(bool(dut.memory.queueEmpty))
      idle(20)
      assert(finished.size == 1 && attempts.size == 1)
      assert(reasonEffects == (if (enabled && !virtual && standard.isEmpty) 1 else 0))
      assert(traps + hsTraps == (if (source.faults.nonEmpty) 1 else 0) && debugTraps == 0)
      assert(hsTraps == (if (delegated) 1 else 0))
      assert(permissionRequests == (if (enabled) 1 else 0) && permissionResponses == permissionRequests)
      event("architectural-case-complete", s"\"case\":\"$label\",\"traps\":${traps + hsTraps}")
    }
    private def enterFromMachine(privilege: Int, target: BigInt): Unit = {
      dut.io.mode.expect(3.U); dut.io.virtualMode.expect(false.B)
      writeCSR(0x341, target, flush = false)
      writeCSR(0x300, BigInt(privilege) << 11, flush = false)
      val returnPc = setupPc
      watched += returnPc
      val beforeReturnRedirect = redirectsSeen.size
      val returnStarted = cycle
      instruction(BigInt(0x30200073))
      until("new architectural source after MRET")(uint(dut.io.mode) == privilege && !bool(dut.io.virtualMode))
      until("this MRET reaches the actual FTQ target")(redirectsSeen.drop(beforeReturnRedirect).exists { entry =>
        entry._1 >= returnStarted && entry._2 == target && seenRename.get(returnPc).exists(_.rob == entry._3)
      })
      idle(12)
      dut.memory.sourcePrivilege.expect(privilege.U); dut.memory.sourceVirtual.expect(false.B)
    }
    private def allowedLoad(pc: BigInt, data: BigInt, privilege: Int): Unit = {
      planned(pc) = ArchitecturalLoad(pc, 0x8000, data, privilege, false, 0, Set.empty)
      packet(pc, Seq(load(10, 0)), tag = true)
      until("allowed source completion and retirement")(finished(pc) && retiredSources(pc))
      until("allowed source queue drain")(bool(dut.memory.queueEmpty))
      idle(12)
    }
    private def configWrite(address: Int, value: BigInt, younger: Boolean): Unit = {
      dut.io.mode.expect(3.U)
      setRegister(6, value)
      val pc = setupPc
      setupPc += 0x40
      watched += pc
      val youngPc = pc + 4
      if (younger) planned(youngPc) = ArchitecturalLoad(youngPc, 0x8000,
        BigInt("7172737475767778", 16), 3, false, 0, Set.empty)
      val beforeResponse = csrResponses
      val beforeRedirects = redirects
      val beforeDistribution = distributionSeen.size
      val words = Seq(csrWrite(address, 6)) ++ (if (younger) Seq(load(12, 0)) else Seq.empty)
      packet(pc, words, tag = true)
      until("configuration writer response")(csrResponses == beforeResponse + 1)
      until("configuration writer retirement and recovery")(retirementCounts(pc) == 1 && redirects == beforeRedirects + 1)
      until("configuration redirect reaches actual FTQ")(redirectsSeen.exists(x => x._2 == pc + 4 && x._1 >= frontendAccepted(pc)._3))
      idle(12)
      val writes = distributionSeen.drop(beforeDistribution)
      assert(writes.size == 1 && writes.head._2 == address && writes.head._3 == value)
      assert(redirects == beforeRedirects + 1)
      dut.memory.mirror(3).expect(value.U)
      assert(!canceledSources(pc), "A software writer was canceled by its own flushAfter")
      if (younger) {
        assert(frontendAccepted.contains(youngPc))
        assert(canceledSources(youngPc) && !retiredSources(youngPc))
        event("configuration-younger-cancel", s"\"writer_pc\":$pc,\"younger_pc\":$youngPc," +
          s"\"renamed\":${seenRename.contains(youngPc)},\"issued\":${sourceKeys.contains(youngPc)}")
      }
      until("configuration recovery queue cleanup")(bool(dut.memory.queueEmpty))
      event("configuration-write-complete", s"\"writer_pc\":$pc,\"address\":$address,\"value\":$value,\"retire_count\":${retirementCounts(pc)}")
    }
    def runRecovery(): Unit = {
      require(enabled, "FDI configuration recovery is an enabled-feature case")
      prepare(0)
      dut.io.criticalErrorState.expect(false.B)
      allowedLoad(0x4000, BigInt("3132333435363738", 16), 0)
      watched += BigInt(0x4100)
      val beforeEntry = traps
      packet(0x4100, Seq(BigInt(0x73)), tag = false)
      until("ordinary ECALL enters M handler")(traps == beforeEntry + 1)
      until("ordinary trap reaches FTQ")(redirectsSeen.exists(_._2 == 0x6000))
      idle(12)
      dut.io.mcause.expect(8.U); dut.io.mepc.expect(0x4100.U)
      dut.io.mode.expect(3.U); dut.memory.reason.expect(7.U)
      assert(reasonEffects == 0 && debugTraps == 0)
      setupPc = 0x6000
      configWrite(0x891, 0x8000, younger = true)
      enterFromMachine(0, 0x5000)
      val deniedPc = BigInt(0x5000)
      planned(deniedPc) = ArchitecturalLoad(deniedPc, 0x8000, BigInt("9192939495969798", 16),
        0, false, 1, Set(24))
      val beforeDenied = traps
      val beforePermission = permissionRequests
      val beforeDeniedRedirect = redirectsSeen.size
      val deniedStarted = cycle
      packet(deniedPc, Seq(load(14, 0)), tag = true)
      until("first protected load uses changed window")(finished(deniedPc) && traps == beforeDenied + 1)
      until("this denied load reaches the actual handler target")(redirectsSeen.drop(beforeDeniedRedirect).exists { entry =>
        entry._1 >= deniedStarted && entry._2 == 0x6000 && seenRename.get(deniedPc).exists(_.rob == entry._3)
      })
      idle(12)
      assert(permissionRequests == beforePermission + 1)
      dut.io.mcause.expect(24.U); dut.io.mepc.expect(deniedPc.U); dut.io.mtval.expect(0x8000.U)
      dut.memory.reason.expect(2.U); assert(reasonEffects == 1)
      until("denied resumed source queue drain")(bool(dut.memory.queueEmpty))
      setupPc = 0x6800
      configWrite(0x891, 0x8008, younger = false)
      configWrite(0x891, 0x8008, younger = false)
      val readPc = setupPc
      setupPc += 0x40
      watched += readPc
      val beforeReadResponse = csrResponses
      val beforeReadRedirects = redirects
      val beforeReadDistribution = distributionSeen.size
      expectedRead = Some(BigInt(0x8008))
      packet(readPc, Seq((BigInt(0x891) << 20) | (BigInt(2) << 12) | (BigInt(9) << 7) | 0x73))
      until("pure bound read response and retirement")(csrResponses == beforeReadResponse + 1 && retirementCounts(readPc) == 1)
      expectedRead = None
      idle(12)
      assert(redirects == beforeReadRedirects && distributionSeen.size == beforeReadDistribution)
      dut.memory.mirror(3).expect(0x8008.U)
      enterFromMachine(0, 0x5200)
      val beforeRestored = permissionRequests
      allowedLoad(0x5200, BigInt("4142434445464748", 16), 0)
      assert(permissionRequests == beforeRestored + 1 && reasonEffects == 1)
      assert(traps == 2 && hsTraps == 0 && debugTraps == 0)
      dut.memory.reason.expect(2.U)
      dut.io.criticalErrorState.expect(false.B)
      event("configuration-recovery-complete", "\"changed_write\":1,\"restoring_write\":1,\"same_value_write\":1,\"pure_read\":1")
    }
    def runMprv(): Unit = {
      prepare(3, mstatusExtra = (BigInt(1) << 17) | (BigInt(1) << 39))
      dut.io.criticalErrorState.expect(false.B)
      dut.memory.sourcePrivilege.expect(3.U); dut.memory.sourceVirtual.expect(false.B)
      dut.memory.dataPrivilege.expect(0.U); dut.memory.dataVirtual.expect(true.B)
      val pc = BigInt(0x4000)
      planned(pc) = ArchitecturalLoad(pc, 0x8010, BigInt("f1e2d3c4b5a69788", 16), 3, false, 0, Set.empty)
      packet(pc, Seq(load(10, 16)), tag = true)
      until("MPRV load completion and retirement")(finished(pc) && retiredSources(pc))
      until("MPRV queue drain")(bool(dut.memory.queueEmpty))
      idle(12)
      assert(traps == 0 && hsTraps == 0 && debugTraps == 0 && reasonEffects == 0)
      assert(permissionRequests == (if (enabled) 1 else 0) && permissionResponses == permissionRequests)
      event("mprv-source-separation-complete", "\"source_privilege\":3,\"source_virtual\":false,\"data_privilege\":0,\"data_virtual\":true")
    }
  }


  // These programs isolate timing opportunities absent from the full suite.
  // All scheduling pressure comes from instructions or public frontend bubbles.
  private class RemainingDriver(module: FDILoadPermissionBackendHarness, name: String, deny: Boolean)
    extends ArchitecturalDriver(module, s"remaining-$name-$deny") {
    private val denied = enabled && deny
    private val debugCase = name == "trigger-debug"
    private val parentPc = BigInt(if (name == "held") 0x4004 else 0x4000)
    private val consumerPc = parentPc + 4
    private val childPc = parentPc + 8
    private val parentAddress = BigInt(if (deny) 0x8080 else 0x8000)
    private val parentData = BigInt(0x8040)
    private val childData = BigInt("6789a1b2c3d4e5f0", 16)
    private val dividend = BigInt("123456789abc", 16)
    private val divisor = BigInt(7)
    private val quotient = dividend / divisor
    private val values = mutable.Map.empty[BigInt, BigInt]
    private val keys = mutable.Map.empty[BigInt, Key]
    private val controlSources = mutable.Set.empty[BigInt]
    private val controlKeys = mutable.Map.empty[BigInt, Key]
    // IFU recovery precedes the FTQ-to-BPU recovery tail for a ROB flush.
    // Entries in this ledger become visible to callers only after edge consumes them.
    private val bpuRecoverySeen = mutable.ArrayBuffer.empty[(Int, BigInt, Pointer)]
    private val loadStart = mutable.Map.empty[BigInt, (Int, Int)]
    private val loadDone = mutable.Set.empty[BigInt]
    private val written = mutable.Set.empty[BigInt]
    private val executed = mutable.Set.empty[BigInt]
    private val retired = mutable.Set.empty[BigInt]
    private val enqueued = mutable.Set.empty[BigInt]
    private val inputOwners = mutable.Map.empty[BigInt, (Pointer, BigInt)]
    private var previousRc = Vector.fill[Option[BigInt]](dut.memory.rcWrites.size)(None)
    // RC write qualification is delayed one cycle per physical port.
    private var previousRcValid = Vector.fill(dut.memory.rcWrites.size)(false)
    private var enqueueCoincidence = false
    private var initialDependency = false
    private var heldDependency = false
    private var heldCanceled = false
    private var firstStoredCancel = -1
    private var canceledSourceBusy = false
    private var parentLd1 = 0
    private var parentLd2 = 0
    private var transferWitness = false
    private var intermediateWake = false
    private var activeAge2Cancel = 0
    private var setupDebugEntries = 0
    private var targetDebugEntries = 0
    private var targetArmed = false
    private def rejected(pc: BigInt): Boolean = denied && pc >= parentPc
    private def owner(rob: xiangshan.backend.rob.RobPtr, destination: UInt): Option[BigInt] =
      keys.collectFirst { case (pc, key) if key == Key(pointer(rob), uint(destination)) => pc }
    private def realDequeue(pc: BigInt): Boolean = dut.memory.iqDequeue.exists(port =>
      bool(port.valid) && bool(port.ready) && owner(port.rob, port.destination).contains(pc))
    private def log(kind: String, detail: String): Unit = event(kind, s"\"case\":\"remaining-$name-$deny\",$detail")

    override def edge(): Unit = {
      dut.memory.response.foreach { response => zero(response.bits); response.valid.poke(false.B) }
      for (item <- pending if item.accepted + 2 == cycle) {
        val value = if (item.address == 0x8048) childData else parentData
        val response = dut.memory.response(item.lane)
        response.valid.poke(true.B); response.bits.id.poke(item.id.U)
        response.bits.data.poke((value | (value << 64)).U)
      }
      for (item <- pending if item.accepted + 3 == cycle) {
        val value = if (item.address == 0x8048) childData else parentData
        dut.memory.response(item.lane).bits.data_delayed.poke((value | (value << 64)).U)
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady) && !bool(dut.io.ifuRedirect.valid)) {
        fetches(ftqPointer(dut.io.fetchRequest.bits.ftqIdx)) = uint(dut.io.fetchRequest.bits.startAddr)
        if (debugCase) log("remaining-debug-ifu-request",
          s"\"pc\":${uint(dut.io.fetchRequest.bits.startAddr)}," +
            s"\"ftq\":${uint(dut.io.fetchRequest.bits.ftqIdx.value)},\"ftq_flag\":${bool(dut.io.fetchRequest.bits.ftqIdx.flag)}")
      }
      if (debugCase && bool(dut.io.ftqToBackend.pc_mem_wen))
        log("remaining-debug-ftq-pc-write", s"\"pc\":${uint(dut.io.ftqToBackend.pc_mem_wdata.startAddr)}," +
          s"\"ftq_index\":${uint(dut.io.ftqToBackend.pc_mem_waddr)}")
      if (debugCase && bool(dut.io.frontendRedirect.valid)) {
        val red = dut.io.frontendRedirect.bits
        bpuRecoverySeen += ((cycle, uint(red.cfiUpdate.target), pointer(red.robIdx)))
        log("remaining-bpu-recovery", s"\"target\":${uint(red.cfiUpdate.target)}," +
          s"\"rob\":${uint(red.robIdx.value)},\"flag\":${bool(red.robIdx.flag)}")
      }
      if (bool(dut.io.csrResponse.valid)) csrResponses += 1
      if (bool(dut.io.backendRedirect.valid)) redirects += 1
      if (bool(dut.io.ifuRedirect.valid)) {
        val red = dut.io.ifuRedirect.bits
        redirectsSeen += ((cycle, uint(red.cfiUpdate.target), pointer(red.robIdx)))
        log("remaining-ftq-redirect", s"\"target\":${uint(red.cfiUpdate.target)},\"rob\":${uint(red.robIdx.value)},\"flag\":${bool(red.robIdx.flag)}")
      }
      if (bool(dut.io.mEntry)) { traps += 1; trapCycle = cycle }
      if (bool(dut.io.hsEntry)) hsTraps += 1
      if (bool(dut.io.debugEntry)) {
        if (targetArmed) targetDebugEntries += 1 else setupDebugEntries += 1
        log("remaining-debug-entry", s"\"target_armed\":$targetArmed")
      }
      if (bool(dut.memory.reasonEffect)) reasonEffects += 1
      dut.io.criticalErrorState.expect(false.B)
      (Seq(dut.io.instruction) ++ dut.io.instructionTail).foreach { port =>
        if (bool(port.valid) && bool(port.ready) && values.contains(uint(port.bits.pc))) {
          val pc = uint(port.bits.pc)
          assert(!inputOwners.contains(pc), "A frontend slot was accepted twice")
          inputOwners(pc) = (ftqPointer(port.bits.ftqPtr), uint(port.bits.ftqOffset))
          log("remaining-frontend", s"\"pc\":$pc,\"ftq\":${uint(port.bits.ftqPtr.value)},\"offset\":${uint(port.bits.ftqOffset)}")
        }
      }
      dut.memory.rename.foreach { port =>
        if (bool(port.valid) && controlSources(uint(port.bits.pc))) {
          val pc = uint(port.bits.pc)
          assert(!controlKeys.contains(pc))
          controlKeys(pc) = Key(pointer(port.bits.robIdx), uint(port.bits.pdest))
        }
        if (bool(port.valid) && values.contains(uint(port.bits.pc))) {
          val pc = uint(port.bits.pc)
          assert(!keys.contains(pc))
          port.bits.rfWen.expect(true.B); port.bits.fpWen.expect(false.B)
          keys(pc) = Key(pointer(port.bits.robIdx), uint(port.bits.pdest))
          log("remaining-rename", s"\"pc\":$pc,\"rob\":${uint(port.bits.robIdx.value)},\"flag\":${bool(port.bits.robIdx.flag)},\"pdest\":${uint(port.bits.pdest)}")
        }
      }
      dut.memory.issue.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) {
          val pc = uint(port.bits.uop.pc)
          assert(pc == parentPc || name == "transitive" && pc == childPc)
          assert(pc == parentPc || !denied, "A denied speculative address reached the real load port")
          assert(keys(pc) == Key(pointer(port.bits.uop.robIdx), uint(port.bits.uop.pdest)))
          val address = if (pc == parentPc) parentAddress else BigInt(0x8048)
          assert(uint(port.bits.src(0)) + uint(port.bits.uop.imm) == address)
          assert(!loadStart.contains(pc)); loadStart(pc) = (cycle, lane)
          log("remaining-load-issue", s"\"pc\":$pc,\"lane\":$lane,\"address\":$address")
        }
        if (bool(dut.memory.requests(lane).valid)) {
          val req = dut.memory.requests(lane).bits
          val address = uint(req.vaddr)
          assert(address == parentAddress || name == "transitive" && address == 0x8048 && !denied)
          pending += Cache(lane, cycle, address, uint(req.id))
        }
      }
      val parent = keys.get(parentPc)
      val lane = loadStart.get(parentPc).map(_._2)
      lane.foreach { loadLane =>
        if (bool(dut.memory.wakeup(loadLane)))
          log("remaining-load-prediction", s"\"parent_pc\":$parentPc,\"lane\":$loadLane")
        if (bool(dut.memory.ld1Cancel(loadLane))) parentLd1 += 1
        if (bool(dut.memory.ld2Cancel(loadLane))) parentLd2 += 1
        if (bool(dut.memory.ld1Cancel(loadLane)) || bool(dut.memory.ld2Cancel(loadLane)))
          log("remaining-load-cancel", s"\"parent_pc\":$parentPc,\"lane\":$loadLane," +
            s"\"ld1\":${bool(dut.memory.ld1Cancel(loadLane))},\"ld2\":${bool(dut.memory.ld2Cancel(loadLane))}")
      }
      dut.memory.iqEnqueue.foreach { port =>
        if (bool(port.valid) && bool(port.ready)) owner(port.rob, port.destination).foreach { pc =>
          enqueued += pc
          if (pc == consumerPc && name == "new-enqueue") {
            val matching = dut.memory.incomingWakeup.filter(wake => bool(wake.valid) && bool(wake.integer) &&
              uint(wake.queue) == uint(port.queue) && parent.exists(_.destination == uint(wake.destination)) &&
              lane.exists(_ == wake.sourceLoadLane.peek().litValue.toInt))
            if (matching.nonEmpty) {
              port.sourceRegister(0).expect(parent.get.destination.U)
              enqueueCoincidence = true
              log("remaining-new-enqueue-wakeup", s"\"consumer_pc\":$pc,\"parent_pc\":$parentPc,\"lane\":${lane.get},\"queue\":${uint(port.queue)}")
            }
          }
        }
      }
      dut.memory.iqState.foreach { port =>
        if (bool(port.valid)) owner(port.rob, port.destination).foreach { pc =>
          for (parentKey <- parent; loadLane <- lane if pc == consumerPc) {
            port.sourceRegister(0).expect(parentKey.destination.U)
            val dependency = uint(port.sourceDependency(0)(loadLane))
            if (name == "new-enqueue" && bool(port.initial) && dependency != 0) initialDependency = true
            if (name == "held" && dependency != 0 && bool(port.sourceUsed(1)) && !bool(port.sourceReady(1)) && !realDequeue(pc)) {
              port.sourceRegister(1).expect(keys(BigInt(0x4000)).destination.U)
              heldDependency = true
              log("remaining-unselected-dependency", s"\"consumer_pc\":$pc,\"lane\":$loadLane,\"dependency\":$dependency,\"parent_ready\":${bool(port.sourceReady(0))}")
            }
            if (denied && bool(port.storedCancel(0))) {
              heldCanceled = true
              if (firstStoredCancel < 0) firstStoredCancel = cycle
              log("remaining-stored-cancel", s"\"consumer_pc\":$pc,\"lane\":$loadLane,\"dependency\":$dependency")
            }
            if (firstStoredCancel >= 0 && cycle > firstStoredCancel && !bool(port.sourceReady(0))) {
              canceledSourceBusy = true
              log("remaining-canceled-source-busy", s"\"consumer_pc\":$pc,\"lane\":$loadLane")
            }
          }
          if (name == "transitive" && pc == childPc && lane.nonEmpty && keys.contains(consumerPc)) {
            port.sourceRegister(0).expect(keys(consumerPc).destination.U)
            val matching = dut.memory.incomingWakeup.filter(wake => bool(wake.valid) && bool(wake.integer) &&
              uint(wake.queue) == uint(port.queue) && uint(wake.destination) == keys(consumerPc).destination &&
              uint(wake.dependency(lane.get)) != 0)
            if (matching.nonEmpty) {
              intermediateWake = true
              if (denied) {
                port.transferCancel(0).expect(true.B)
                assert(!realDequeue(pc), "Canceled speculative wake selected its descendant")
                transferWitness = true
              }
              log("remaining-intermediate-wakeup", s"\"intermediate_pc\":$consumerPc,\"descendant_pc\":$pc," +
                s"\"lane\":${lane.get},\"dependency\":${uint(matching.head.dependency(lane.get))}," +
                s"\"transfer_cancel\":${bool(port.transferCancel(0))},\"observation\":\"iq_wakeup\"")
            }
          }
        }
      }
      for (loadLane <- lane) {
        for ((ports, phase) <- Seq(dut.memory.dataInput -> "datapath-input", dut.memory.dataOutput -> "datapath-output")) {
          ports.foreach { port =>
            if (bool(port.valid)) owner(port.rob, port.destination).foreach { pc =>
              if (pc >= consumerPc) {
                val dependency = uint(port.dependency(loadLane))
                val cancel1 = dependency.testBit(0) && bool(dut.memory.ld1Cancel(loadLane))
                val cancel2 = dependency.testBit(1) && bool(dut.memory.ld2Cancel(loadLane))
                if (denied && phase == "datapath-input" && bool(port.ready) && cancel2) activeAge2Cancel += 1
                if (denied && phase == "datapath-output" && bool(port.ready))
                  assert(false, "A denied dependent passed the DataPath cancellation boundary")
                log("remaining-phase", s"\"pc\":$pc,\"phase\":\"$phase\",\"ready\":${bool(port.ready)}," +
                  s"\"lane\":$loadLane,\"dependency\":$dependency,\"cancel1\":$cancel1,\"cancel2\":$cancel2")
              }
            }
          }
        }
      }
      dut.memory.completion.zipWithIndex.foreach { case (port, loadLane) =>
        if (bool(port.valid)) {
          val pc = uint(port.bits.uop.pc)
          assert(keys(pc) == Key(pointer(port.bits.uop.robIdx), uint(port.bits.uop.pdest)))
          assert(loadDone.add(pc))
          val blocked = rejected(pc)
          port.bits.data.expect((if (blocked) BigInt(0) else values(pc)).U)
          port.bits.uop.rfWen.expect((!blocked).B); port.bits.uop.fpWen.expect(false.B)
          port.bits.uop.exceptionVec.zipWithIndex.foreach { case (bit, cause) => bit.expect((blocked && cause == 24).B) }
          if (debugCase) port.bits.uop.trigger.expect(xiangshan.TriggerAction.DebugMode)
          else port.bits.uop.trigger.expect(xiangshan.TriggerAction.None)
          if (blocked) {
            port.bits.uop.fdiException.get.tval.expect(parentAddress.U)
            port.bits.uop.fdiException.get.reason.expect(2.U)
            val bypass = dut.memory.bypass.filter(port => uint(port.destination) == keys(pc).destination)
            assert(bypass.nonEmpty); bypass.foreach { port => port.valid.expect(false.B); port.data.expect(0.U) }
          }
          val wb = dut.memory.wb.filter(w => bool(w.fire) && pointer(w.rob) == keys(pc).rob)
          assert(wb.size == 1); wb.head.intWen.expect((!blocked).B); wb.head.fpWen.expect(false.B)
          log("remaining-load-completion", s"\"pc\":$pc,\"lane\":$loadLane,\"blocked\":$blocked,\"trigger\":${uint(port.bits.uop.trigger)}")
        }
      }
      dut.memory.execution.foreach { port =>
        if (bool(port.valid) && !bool(port.bits.floating)) owner(port.bits.rob, port.bits.destination).foreach { pc =>
          assert(!rejected(pc), "Denied data reached a real FU")
          if (pc == consumerPc) port.bits.operand.expect(parentData.U)
          assert(executed.add(pc))
          log("remaining-fu-accept", s"\"pc\":$pc")
        }
      }
      dut.memory.registerWrites.foreach { port =>
        if (bool(port.valid) && !bool(port.floating)) keys.find(_._2.destination == uint(port.destination)).foreach { case (pc, _) =>
          assert(!rejected(pc), "Denied data wrote the integer RF")
          port.data.expect(values(pc).U); assert(written.add(pc))
        }
        if (bool(port.valid) && bool(port.floating))
          assert(false, "The integer-only remaining program unexpectedly wrote the FP RF")
      }
      dut.memory.rcWrites.zipWithIndex.foreach { case (port, index) =>
        if (bool(port.valid)) {
          assert(previousRcValid(index), "An RC write had no valid producer on the preceding cycle")
          previousRc(index).foreach { pc =>
            assert(!rejected(pc), "Denied data wrote the register cache")
            port.data.expect(values(pc).U)
          }
        }
      }
      previousRc = dut.memory.rcWrites.map { port =>
        if (bool(port.sourceValid)) keys.collectFirst { case (pc, key) if key.destination == uint(port.sourceDestination) => pc }
        else None
      }.toVector
      previousRcValid = dut.memory.rcWrites.map(port => bool(port.sourceValid)).toVector
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid)) keys.collectFirst { case (pc, key) if key.rob == pointer(port.bits) => pc }.foreach { pc =>
          assert(!rejected(pc), "The denied source or descendant retired")
          assert(retired.add(pc), "A source retired more than once")
        }
      }
      dut.memory.uncacheRequest.expect(false.B); dut.memory.splitRequest.foreach(_.expect(false.B))
      dut.clock.step(); cycle += 1
    }

    private def prepareRemaining(): Unit = {
      prepare(3)
      if (enabled) writeCSR(0x891, 0x8050, flush = true)
      if (name == "held") { setRegister(2, dividend); setRegister(3, divisor) }
      writeCSR(0x341, 0x4000, flush = false)
      writeCSR(0x300, 0, flush = false)
      val before = redirectsSeen.size
      val returnPc = setupPc
      controlSources += returnPc
      instruction(BigInt(0x30200073))
      until("remaining HU return and real FTQ target") {
        uint(dut.io.mode) == 0 && !bool(dut.io.virtualMode) && redirectsSeen.drop(before).exists(x => x._2 == 0x4000 && controlKeys.get(returnPc).exists(_.rob == x._3))
      }
      idle(12)
      dut.memory.sourcePrivilege.expect(0.U); dut.memory.sourceVirtual.expect(false.B)
      if (enabled) {
        dut.memory.reason.expect(7.U)
        dut.memory.mirror(0).expect(2.U); dut.memory.mirror(1).expect(10.U)
        dut.memory.mirror(2).expect(0x8000.U); dut.memory.mirror(3).expect(0x8050.U)
      }
      assert(reasonEffects == 0 && traps == 0 && hsTraps == 0 && setupDebugEntries == 0)
    }

    private def splitPacket(words: Seq[BigInt]): Unit = {
      require(words.size == 2)
      val pc = parentPc
      val ptr = ftqPointer(dut.io.ftqNext)
      def predict(stage: BranchPredictionBundle): Unit = {
        zero(stage); stage.pc.foreach(_.poke(pc.U)); stage.valid.foreach(_.poke(true.B)); setPointer(stage.ftq_idx, ptr)
      }
      fetches -= ptr; zero(dut.io.bpu.resp.bits)
      predict(dut.io.bpu.resp.bits.s1); dut.io.bpu.resp.valid.poke(true.B)
      until("split packet BPU ready")(bool(dut.io.bpu.resp.ready)); edge()
      dut.io.bpu.resp.valid.poke(false.B); zero(dut.io.bpu.resp.bits.s1)
      predict(dut.io.bpu.resp.bits.s2); edge(); zero(dut.io.bpu.resp.bits.s2)
      predict(dut.io.bpu.resp.bits.s3); edge(); zero(dut.io.bpu.resp.bits.s3)
      until("split packet actual fetch")(fetches.contains(ptr)); assert(fetches(ptr) == pc)
      zero(dut.io.predecode.bits); setPointer(dut.io.predecode.bits.ftqIdx, ptr)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (value, index) => value.poke((pc + 2 * index).U) }
      words.indices.foreach { index =>
        dut.io.predecode.bits.pd(2 * index).valid.poke(true.B)
        dut.io.predecode.bits.instrRange(2 * index).poke(true.B)
      }
      dut.io.predecode.bits.ftqOffset.poke(2.U); dut.io.predecode.bits.target.poke((pc + 8).U)
      dut.io.predecode.valid.poke(true.B); edge(); dut.io.predecode.valid.poke(false.B)
      val input = dut.io.instruction
      dut.io.instructionTail.foreach(_.valid.poke(false.B))
      var firstAccepted = -1
      words.zipWithIndex.foreach { case (word, index) =>
        // The five-cycle interval is between actual frontend acceptances.
        if (index == 1) while (cycle < firstAccepted + 5) edge()
        zero(input.bits); input.valid.poke(true.B)
        input.bits.instr.poke(word.U); input.bits.pc.poke((pc + 4 * index).U)
        input.bits.pd.valid.poke(true.B); input.bits.trigger.poke(xiangshan.TriggerAction.None)
        input.bits.fdiNotTrusted.foreach(_.poke(true.B)); setPointer(input.bits.ftqPtr, ptr)
        input.bits.ftqOffset.poke((2 * index).U); input.bits.isLastInFtqEntry.poke((index == 1).B)
        until("split packet real decode acceptance")(bool(input.ready))
        if (index == 0) firstAccepted = cycle
        edge(); input.valid.poke(false.B)
      }
    }

    private def configureTrigger(): Unit = {
      dut.io.mode.expect(0.U); dut.io.debugMode.expect(false.B)
      dut.io.interrupts.debug.poke(true.B)
      until("normal haltreq selected")(bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.debug))
      val haltPc = BigInt(0x3f00)
      controlSources += haltPc
      val before = redirectsSeen.size
      val beforeBpuRecovery = bpuRecoverySeen.size
      packet(haltPc, Seq(addi(0, 0, 0)))
      until("HU haltreq reaches Debug and consumed IFU/BPU recovery") {
        def matchesHalt(entry: (Int, BigInt, Pointer)): Boolean =
          entry._2 == dut.debugEntryAddress && controlKeys.get(haltPc).exists(_.rob == entry._3)
        setupDebugEntries == 1 && bool(dut.io.debugMode) &&
          redirectsSeen.drop(before).exists(matchesHalt) &&
          bpuRecoverySeen.drop(beforeBpuRecovery).exists(matchesHalt)
      }
      dut.io.dpc.expect(haltPc.U)
      assert((uint(dut.io.dcsr) & 3) == 0 && ((uint(dut.io.dcsr) >> 6) & 7) == 3)
      dut.io.interrupts.debug.poke(false.B)
      setupPc = dut.debugEntryAddress
      writeCSR(0x7a0, 0, flush = false)
      writeCSR(0x7a2, parentAddress, flush = false)
      val control = (BigInt(6) << 60) | (BigInt(1) << 59) | (BigInt(1) << 12) | 9
      writeCSR(0x7a1, control, flush = false)
      writeCSR(0x7b1, parentPc, flush = false)
      // Preserve the real HU return mode saved by haltreq; DRET does not choose it.
      assert((uint(dut.io.dcsr) & 3) == 0)
      val beforeReturn = redirectsSeen.size
      val returnPc = setupPc
      controlSources += returnPc
      instruction(BigInt("7b200073", 16))
      until("DRET returns to the actual HU source and FTQ") {
        !bool(dut.io.debugMode) && uint(dut.io.mode) == 0 && !bool(dut.io.virtualMode) &&
          redirectsSeen.drop(beforeReturn).exists(x => x._2 == parentPc && controlKeys.get(returnPc).exists(_.rob == x._3))
      }
      idle(12)
      dut.memory.sourcePrivilege.expect(0.U); dut.memory.sourceVirtual.expect(false.B)
      dut.memory.reason.expect(7.U)
      assert(setupDebugEntries == 1 && targetDebugEntries == 0 && reasonEffects == 0)
    }

    def runRemaining(): Unit = {
      prepareRemaining()
      if (debugCase) { require(enabled && denied); configureTrigger() }
      values(parentPc) = parentData
      values(consumerPc) = if (name == "held") parentData + quotient else if (name == "transitive") parentData + 8 else parentData + 1
      if (name == "held") values(BigInt(0x4000)) = quotient
      if (name == "transitive") values(childPc) = childData
      targetArmed = true
      val parentWord = load(10, if (deny) 128 else 0)
      if (name == "new-enqueue") splitPacket(Seq(parentWord, addi(11, 10, 1)))
      else if (name == "held") {
        val div = (BigInt(1) << 25) | (BigInt(3) << 20) | (BigInt(2) << 15) | (BigInt(4) << 12) | (BigInt(21) << 7) | 0x33
        val add = (BigInt(21) << 20) | (BigInt(10) << 15) | (BigInt(11) << 7) | 0x33
        packet(0x4000, Seq(div, parentWord, add), tag = true)
      } else if (name == "transitive") {
        val child = (BigInt(11) << 15) | (BigInt(3) << 12) | (BigInt(12) << 7) | 3
        packet(parentPc, Seq(parentWord, addi(11, 10, 8), child), tag = true)
      } else packet(parentPc, Seq(parentWord, addi(11, 10, 1)), tag = true)
      if (denied) {
        if (debugCase) {
          until("target trigger Debug and source FTQ recovery") {
            targetDebugEntries == 1 && bool(dut.io.debugMode) &&
              redirectsSeen.exists(x => x._2 == dut.debugEntryAddress && keys.get(parentPc).exists(_.rob == x._3))
          }
          idle(12)
          dut.io.dpc.expect(parentPc.U)
          assert(((uint(dut.io.dcsr) >> 6) & 7) == 2 && (uint(dut.io.dcsr) & 3) == 0)
          dut.memory.reason.expect(7.U)
          assert(traps == 0 && hsTraps == 0 && reasonEffects == 0 && setupDebugEntries == 1)
        } else {
          until("remaining denied precise trap")(traps == 1)
          idle(12)
          dut.io.mcause.expect(24.U); dut.io.mepc.expect(parentPc.U); dut.io.mtval.expect(parentAddress.U)
          dut.memory.reason.expect(2.U)
          assert(reasonEffects == 1 && hsTraps == 0 && targetDebugEntries == 0)
        }
        assert(loadDone == Set(parentPc) && written.forall(_ < parentPc) && executed.forall(_ < parentPc))
      } else {
        until("remaining allowed result and retirement")(written.size == values.size && retired.size == values.size)
        assert(traps == 0 && hsTraps == 0 && targetDebugEntries == 0 && reasonEffects == 0)
      }
      until("remaining actual LQ cleanup")(bool(dut.memory.queueEmpty)); idle(20)
      assert(inputOwners.keySet == values.keySet && enqueued.contains(consumerPc))
      if (name == "new-enqueue") {
        assert(enqueueCoincidence && initialDependency,
          "The fixed legal frontend schedule did not exercise actual new-entry wakeup")
        if (denied) assert(heldCanceled && canceledSourceBusy,
          "New-entry cancellation was not observed before its later ROB flush")
      }
      if (name == "held") {
        assert(heldDependency, "The consumer did not reside with an unresolved second source and parent dependency")
        if (denied) assert(heldCanceled && canceledSourceBusy)
        assert(retired(BigInt(0x4000)) && written(BigInt(0x4000)))
      }
      if (name == "transitive") {
        assert(enqueued(childPc) && intermediateWake, "No actual intermediate dependency wake reached the descendant queue")
        if (denied) assert(transferWitness)
        else assert(loadDone == Set(parentPc, childPc) && executed(consumerPc))
      }
      assert(pending.size == loadStart.size && loadDone.size == loadStart.size)
      assert(traps == (if (denied && !debugCase) 1 else 0) && hsTraps == 0)
      assert(reasonEffects == (if (denied && !debugCase) 1 else 0))
      if (denied) assert(parentLd1 > 0 && parentLd2 > 0)
      assert(targetDebugEntries == (if (debugCase) 1 else 0))
      log("remaining-case-complete", s"\"denied\":$denied,\"new_enqueue\":$enqueueCoincidence," +
        s"\"held_dependency\":$heldDependency,\"intermediate_wake\":$intermediateWake," +
        s"\"transfer_cancel\":$transferWitness,\"active_age2_cancel\":$activeAge2Cancel," +
        s"\"target_debug_entries\":$targetDebugEntries,\"setup_debug_entries\":$setupDebugEntries")
    }
  }

  it should "block denied data while preserving the actual ROB exception completion" in {
    val root = Paths.get(sys.props("l01.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Use the dedicated L01 run directory")
    val path = root.resolve(if (enabled) "backend-enabled" else "backend-disabled")
    require(!Files.exists(path)); Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
    val options = p(xiangshan.DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDILoadPermissionBackendHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      val driver = new Driver(elaborated.wrapped)
      try {
        if (scenario == "full") {
          for (floating <- Seq(false, true); denied <- Seq(false, true)) {
            val consumer = new ConsumerDriver(elaborated.wrapped, floating, denied)
            try consumer.runConsumers()
            finally driver.events ++= consumer.events
          }
          println(s"L01_BACKEND_CONSUMERS_PASS enabled=$enabled cases=4")
          val faultCases = Seq("supervisor", "guest", "late-error", "pmp-error") ++
            (if (enabled) Seq("supervisor-delegated", "mmio") else Seq.empty)
          for (name <- faultCases) {
            val architectural = new ArchitecturalDriver(elaborated.wrapped, name)
            try architectural.runFault(name)
            finally driver.events ++= architectural.events
          }
          println(s"L01_BACKEND_FAULTS_PASS enabled=$enabled cases=${faultCases.size}")
          if (enabled) {
            val recovery = new ArchitecturalDriver(elaborated.wrapped, "configuration-recovery")
            try recovery.runRecovery()
            finally driver.events ++= recovery.events
          }
          val mprv = new ArchitecturalDriver(elaborated.wrapped, "mprv-source-separation")
          try mprv.runMprv()
          finally driver.events ++= mprv.events
          println(s"L01_BACKEND_RECOVERY_PASS enabled=$enabled configurationCases=${if (enabled) 1 else 0} mprvCases=1")
        } else if (scenario == "remaining") {
          var cases = 0
          for (name <- Seq("new-enqueue", "held", "transitive"); deny <- (if (enabled) Seq(false, true) else Seq(false))) {
            val remaining = new RemainingDriver(elaborated.wrapped, name, deny)
            try remaining.runRemaining()
            finally driver.events ++= remaining.events
            cases += 1
          }
          if (enabled) {
            val trigger = new RemainingDriver(elaborated.wrapped, "trigger-debug", true)
            try trigger.runRemaining()
            finally driver.events ++= trigger.events
            cases += 1
          }
          println(s"L01_BACKEND_REMAINING_PASS enabled=$enabled cases=$cases")
        } else {
          driver.reset(); driver.minimum()
          println(s"L01_BACKEND_MINIMAL_PASS enabled=$enabled cycles=${driver.cycle} " +
            s"allowed=${driver.allowedWb} denied=${driver.deniedWb} traps=${driver.traps} " +
            s"followingWb=${driver.followingWb} followingRetire=${driver.followingRetire} " +
            "load=production queue=production bypass=production ptw-cache=external-services")
        }
      } finally {
        Files.write(path.resolve("load-events.jsonl"), driver.events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
