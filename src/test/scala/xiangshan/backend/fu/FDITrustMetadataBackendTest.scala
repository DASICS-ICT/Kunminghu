// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{BranchPredictionBundle, FtqPtr}

class FDITrustMetadataBackendTest extends AnyFlatSpec {
  behavior of "FDI metadata at production backend consumers"

  private val scenario = sys.env.getOrElse("C08_BACKEND_SCENARIO", "full")
  private val enabled = sys.env.getOrElse("C08_FDI_ENABLED", "true").toBoolean
  require(Set("minimal", "full").contains(scenario))
  private case class Ptr(flag: Boolean, value: BigInt)
  private case class Key(rob: Ptr, pdest: BigInt, operation: BigInt)
  private case class Instruction(bits: BigInt, tag: Boolean, bytes: Int = 4)
  private case class StoreDataAlias(original: Key, projected: Key, uop: Int)
  private val amocasDInstruction = (BigInt(5) << 27) | (BigInt(6) << 20) | (BigInt(2) << 15) |
    (BigInt(3) << 12) | (BigInt(4) << 7) | 0x2f
  private class Source(val serial: Int, val pc: BigInt, val instruction: Instruction) {
    var accepted = -1
    val renamed = mutable.Set.empty[Int]
    val renamedTokens = mutable.Set.empty[(Int, Key)]
    val keys = mutable.Set.empty[Key]
    val enqueued = mutable.Map.empty[Key, Int]
    val issues = mutable.Map.empty[Key, Int]
    var fuCount = 0
    val fuUnits = mutable.Set.empty[Int]
    val fuAccepts = mutable.ArrayBuffer.empty[(Int, Int, Key)]
    val pipeStages = mutable.Set.empty[(Int, Int, Key)]
    var pipeCount = 0
    var delayedPipeCount = 0
    var memorySamples = 0
    var memoryFires = 0
    val memoryAccepts = mutable.ArrayBuffer.empty[(String, Key)]
    var retries = 0
    var retired = false
  }
  private case class Fusion(firstPC: BigInt, secondPC: BigInt, allowed: Boolean, selector: Int) {
    var samples = 0
  }
  private def addi(rd: Int, rs: Int, imm: Int): BigInt =
    (BigInt(imm & 0xfff) << 20) | (BigInt(rs) << 15) | (BigInt(rd) << 7) | 0x13
  private def shift(rd: Int, rs: Int, amount: Int, right: Boolean = false): BigInt =
    (BigInt(amount) << 20) | (BigInt(rs) << 15) | (BigInt(if (right) 5 else 1) << 12) | (BigInt(rd) << 7) | 0x13
  private def register(rd: Int, rs1: Int, rs2: Int, funct3: Int = 0, funct7: Int = 0): BigInt =
    (BigInt(funct7) << 25) | (BigInt(rs2) << 20) | (BigInt(rs1) << 15) |
      (BigInt(funct3) << 12) | (BigInt(rd) << 7) | 0x33

  private class Driver(val dut: FDITrustMetadataBackendHarness) {
    var cycles = 0
    var nextSerial = 0
    var redirects = 0
    var renameStalls = 0
    val events = mutable.ArrayBuffer.empty[String]
    val sources = mutable.Map.empty[BigInt, Source]
    val keys = mutable.Map.empty[Key, Source]
    val fetches = mutable.Map.empty[Ptr, BigInt]
    val storeDataAliases = mutable.Map.empty[(Int, Key), StoreDataAlias]
    val storeDataIssued = mutable.Set.empty[(Int, Key)]
    val fusion = mutable.Map.empty[BigInt, Fusion]
    val totals = mutable.Map.empty[String, Int].withDefaultValue(0)
    def bool(value: Bool): Boolean = value.peek().litToBoolean
    def uint(value: UInt): BigInt = value.peek().litValue
    def ptr(value: RobPtr): Ptr = Ptr(bool(value.flag), uint(value.value))
    def ptr(value: FtqPtr): Ptr = Ptr(bool(value.flag), uint(value.value))
    def key(value: FDITrustBackendToken): Key = Key(ptr(value.robIdx), uint(value.pdest), uint(value.fuOpType))
    def zero(data: Data): Unit = data match {
      case value: Bool => value.poke(false.B)
      case value: UInt => value.poke(0.U)
      case value: SInt => value.poke(0.S)
      case value: Vec[_] => value.foreach(zero)
      case value: Record => value.elements.values.foreach(zero)
      case other => throw new IllegalArgumentException(s"Unsupported input ${other.getClass.getName}")
    }
    def setPtr(field: FtqPtr, value: Ptr): Unit = {
      field.flag.poke(value.flag.B)
      field.value.poke(value.value.U)
    }
    def record(kind: String, source: Source, detail: String = ""): Unit = {
      totals(kind) += 1
      events += s"{\"cycle\":$cycles,\"event\":\"$kind\",\"source\":${source.serial}," +
        s"\"pc\":${source.pc},\"instruction\":${source.instruction.bits},\"expected_tag\":${enabled && source.instruction.tag}" +
        (if (detail.isEmpty) "}" else "," + detail + "}")
    }
    def check(token: FDITrustBackendToken, kind: String, detail: String = ""): Source =
      checkOriginal(token, kind, key(token), detail)
    private def checkOriginal(token: FDITrustBackendToken, kind: String, original: Key, detail: String): Source = {
      val id = key(token)
      val source = keys.getOrElse(original, throw new AssertionError(s"$kind without a source association: $original at $cycles"))
      token.tag.expect((enabled && source.instruction.tag).B)
      record(kind, source, s"\"rob_flag\":${id.rob.flag},\"rob\":${id.rob.value},\"pdest\":${id.pdest}," +
        s"\"operation\":${id.operation},\"vuop\":${uint(token.vuopIdx)},\"observed_tag\":${bool(token.tag)}" +
        (if (detail.isEmpty) "" else "," + detail))
      source
    }
    private def registerStoreData(queue: Int, payload: FDITrustBackendSource, source: Source): Unit = {
      val original = key(payload.token)
      val index = uint(payload.uopIdx).toInt
      val actualQueue = dut.queues(queue)
      assert(actualQueue.params.StdCnt > 0 && actualQueue.params.exuBlockParams.forall(_.wbPregIdxWidth == 0))
      assert(actualQueue.deqBeforeDly.forall(_.bits.common.pdest.getWidth == 0))
      // This alias is specific to the two independently planned AMOCAS.D uops.
      // STD has no writeback destination; its full opcode remains unchanged.
      assert(source.instruction.bits == amocasDInstruction && Set(0, 1)(index))
      assert(original.operation == (if (index == 0) 111 else 47))
      assert(source.renamedTokens((index, original)))
      val alias = StoreDataAlias(original, original.copy(pdest = 0), index)
      val association = (queue, alias.projected)
      storeDataAliases.get(association).foreach(previous => assert(previous == alias, "Ambiguous STD projection"))
      storeDataAliases(association) = alias
      record("store-data-alias", source, s"\"queue\":$queue,\"role\":\"store-data\",\"source_uop\":$index," +
        s"\"rob_flag\":${original.rob.flag},\"rob\":${original.rob.value},\"original_pdest\":${original.pdest}," +
        s"\"projected_pdest\":0,\"original_operation\":${original.operation},\"projected_operation\":${alias.projected.operation}")
    }
    private def checkStoreData(token: FDITrustBackendToken, kind: String, queue: Int): (Source, Key) = {
      assert(dut.queues(queue).params.StdCnt > 0)
      val observed = key(token)
      val alias = storeDataAliases.getOrElse((queue, observed),
        throw new AssertionError(s"$kind without an observed STD enqueue alias: queue=$queue key=$observed"))
      assert(observed == alias.original.copy(pdest = 0), "STD projection changed a ROB or opcode field")
      val source = checkOriginal(token, kind, alias.original,
        s"\"queue\":$queue,\"role\":\"store-data\",\"source_uop\":${alias.uop}," +
          s"\"original_pdest\":${alias.original.pdest},\"original_operation\":${alias.original.operation}")
      assert(source.renamedTokens((alias.uop, alias.original)))
      (source, alias.original)
    }
    def sourceOf(payload: FDITrustBackendSource): Source = {
      val pc = uint(payload.pc)
      val source = sources.getOrElse(pc, throw new AssertionError(s"Unplanned source PC $pc at $cycles"))
      assert(source.accepted >= 0, s"Unaccepted source reached the backend: $pc")
      payload.instr.expect(source.instruction.bits.U)
      payload.token.tag.expect((enabled && source.instruction.tag).B)
      source
    }
    def edge(): Unit = {
      assert(!bool(dut.io.robException.valid), s"Unexpected architectural exception at cycle $cycles")
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady))
        fetches(ptr(dut.io.fetchRequest.bits.ftqIdx)) = uint(dut.io.fetchRequest.bits.startAddr)
      if (bool(dut.io.backendRedirect.valid)) redirects += 1
      dut.metadata.rename.foreach { port =>
        if (bool(port.valid)) {
          val source = sourceOf(port.bits)
          val id = key(port.bits.token)
          keys.get(id).foreach(previous => assert(previous eq source,
            s"Ambiguous source identity $id for ${previous.pc} and ${source.pc}"))
          keys(id) = source
          source.keys += id
          val index = uint(port.bits.uopIdx).toInt
          source.renamed += index
          // VSET helper uops can share uopIdx while writing distinct register files.
          assert(source.renamedTokens.add((index, id)), "A source transaction renamed more than once")
          record("rename", source, s"\"uop\":${uint(port.bits.uopIdx)},\"pdest\":${id.pdest},\"rob\":${id.rob.value}")
        }
      }
      dut.metadata.renameHeld.foreach { port =>
        if (bool(port.valid)) { sourceOf(port.bits); renameStalls += 1 }
      }
      dut.metadata.fusion.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) fusion.get(uint(port.pc)).foreach { expectation =>
          port.fused.expect(expectation.allowed.B)
          port.clearNext.expect(expectation.allowed.B)
          port.info.expect((if (expectation.allowed) expectation.selector else 0).U)
          port.lsrc2Valid.expect(expectation.allowed.B)
          // These packets place the candidate first, excluding same-cycle RAT bypass.
          assert(lane == 0, "Fusion operand oracle requires the pair in the first lane")
          // The selector is a test-pattern constant, independent of DUT info.
          val expectedPsrc1 = if (!expectation.allowed) uint(port.normalPsrc1)
          else expectation.selector match {
            case 1 => BigInt(0)
            case 2 => uint(port.secondIntRs1)
            case 4 => uint(port.secondIntRs2)
            case other => throw new AssertionError(s"Unknown fusion selector $other")
          }
          if (expectation.allowed && expectation.selector != 1) {
            assert(expectedPsrc1 != 0, "Fusion source initialization did not create a nonzero RAT mapping")
            assert(uint(port.secondIntRs1) != uint(port.secondIntRs2),
              "Fusion source operands must have distinct RAT mappings")
          }
          port.actualPsrc1.expect(expectedPsrc1.U)
          expectation.samples += 1
          record("fusion-decision", sources(expectation.firstPC),
            s"\"allowed\":${expectation.allowed},\"fused\":${bool(port.fused)},\"clear_next\":${bool(port.clearNext)}," +
              s"\"info\":${uint(port.info)},\"lsrc2_valid\":${bool(port.lsrc2Valid)}," +
              s"\"normal_psrc1\":${uint(port.normalPsrc1)},\"second_rs1\":${uint(port.secondIntRs1)}," +
              s"\"second_rs2\":${uint(port.secondIntRs2)},\"expected_psrc1\":$expectedPsrc1," +
              s"\"actual_psrc1\":${uint(port.actualPsrc1)}")
        }
      }
      dut.metadata.enqueue.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) {
          val queue = dut.enqueueQueues(lane)
          val source = check(port.bits.token, "iq-enqueue", s"\"queue\":$queue")
          port.bits.instr.expect(source.instruction.bits.U)
          source.enqueued.getOrElseUpdate(key(port.bits.token), cycles)
          if (dut.queues(queue).params.StdCnt > 0) registerStoreData(queue, port.bits, source)
        }
      }
      dut.metadata.issue.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) {
          val queue = dut.issueQueues(lane)
          val (source, id) = if (dut.queues(queue).params.StdCnt > 0) {
            val checked = checkStoreData(port.bits, "iq-issue", queue)
            storeDataIssued += ((queue, key(port.bits)))
            checked
          } else (check(port.bits, "iq-issue", s"\"queue\":$queue"), key(port.bits))
          source.issues(id) = source.issues.getOrElse(id, 0) + 1
          if (!bool(dut.metadata.issueFirst(lane))) source.retries += 1
        }
      }
      dut.metadata.fu.zipWithIndex.foreach { case (port, unitId) =>
        if (bool(port.valid)) {
          val source = check(port.bits, "fu-control",
            s"\"unit\":$unitId,\"fu_name\":\"${dut.units(unitId).cfg.name}\"")
          source.fuCount += 1
          source.fuUnits += unitId
          source.fuAccepts += ((unitId, uint(port.bits.vuopIdx).toInt, key(port.bits)))
        }
      }
      dut.metadata.pipe.zipWithIndex.foreach { case (port, index) =>
        if (bool(port.valid)) {
          val (unit, pipeline, stage) = dut.piped(index)
          val unitId = dut.units.indexOf(unit)
          val source = check(port.bits, "fu-control-pipe",
            s"\"unit\":$unitId,\"fu_name\":\"${unit.cfg.name}\",\"stage\":$stage," +
              s"\"terminal_stage\":${pipeline.validVec.size - 1},\"selected_fu_valid\":true")
          source.pipeCount += 1
          source.pipeStages += ((unitId, stage, key(port.bits)))
          if (stage > 0) source.delayedPipeCount += 1
        }
      }
      dut.metadata.memory.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid)) {
          val role = dut.memoryRoles(lane)
          val (source, original) = dut.memoryQueues(lane) match {
            case Some(queue) =>
              assert(storeDataIssued((queue, key(port.bits))), "STD memory input preceded its real IQ issue")
              checkStoreData(port.bits, "memory-issue", queue)
            case None => (check(port.bits, "memory-issue", s"\"port\":$lane,\"role\":\"$role\""), key(port.bits))
          }
          source.memorySamples += 1
          if (bool(dut.metadata.memoryFire(lane))) {
            source.memoryFires += 1
            source.memoryAccepts += ((role, original))
            record("memory-fire", source, s"\"port\":$lane,\"role\":\"$role\",\"original_pdest\":${original.pdest}," +
              s"\"original_operation\":${original.operation},\"observed_pdest\":${uint(port.bits.pdest)}," +
              s"\"observed_operation\":${uint(port.bits.fuOpType)}")
          }
        }
      }
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid)) {
          val retired = ptr(port.bits)
          sources.values.filter(_.keys.exists(_.rob == retired)).foreach(_.retired = true)
        }
      }
      dut.clock.step()
      cycles += 1
    }
    def idle(count: Int): Unit = (0 until count).foreach(_ => edge())
    def until(description: String, maximum: Int = 2500)(condition: => Boolean): Unit = {
      var waited = 0
      while (!condition && waited < maximum) { edge(); waited += 1 }
      assert(condition, s"Timed out waiting for $description at cycle $cycles")
    }
    def reset(): Unit = {
      sources.clear(); keys.clear(); fetches.clear(); fusion.clear()
      storeDataAliases.clear(); storeDataIssued.clear()
      redirects = 0
      (Seq(dut.io.instruction) ++ dut.io.instructionTail).foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits); zero(dut.io.predecode); zero(dut.io.interrupts)
      dut.io.fetchReady.poke(true.B)
      dut.io.traceEnable.poke(false.B); dut.io.traceStall.poke(false.B)
      dut.metadata.memoryReady.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(10)
      dut.reset.poke(false.B); dut.clock.step(400)
      cycles += 410
      dut.io.mode.expect(3.U)
    }
    private def prediction(stage: BranchPredictionBundle, pc: BigInt, pointer: Ptr): Unit = {
      zero(stage)
      stage.pc.foreach(_.poke(pc.U)); stage.valid.foreach(_.poke(true.B))
      setPtr(stage.ftq_idx, pointer)
    }
    // The driver copies the normal BPU stage/FTQ/predecode handshake from C07.
    // Tag values are independent stimulus, and are never calculated from DUT state.
    def packet(pc: BigInt, instructions: Seq[Instruction]): Seq[Source] = {
      val lanes = Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq
      require(instructions.nonEmpty && instructions.size <= lanes.size)
      val pcs = instructions.scanLeft(pc)((next, instruction) => next + instruction.bytes).dropRight(1)
      val offsets = pcs.map(p => ((p - pc) / 2).toInt)
      require(offsets.last < dut.io.predecode.bits.pd.length)
      val planned = pcs.zip(instructions).map { case (address, instruction) =>
        assert(!sources.contains(address), "Each source PC must be unique between resets")
        val source = new Source(nextSerial, address, instruction)
        nextSerial += 1
        sources(address) = source
        source
      }
      val pointer = ptr(dut.io.ftqNext)
      fetches -= pointer
      zero(dut.io.bpu.resp.bits)
      prediction(dut.io.bpu.resp.bits.s1, pc, pointer)
      dut.io.bpu.resp.valid.poke(true.B)
      until("BPU acceptance")(bool(dut.io.bpu.resp.ready))
      edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1); prediction(dut.io.bpu.resp.bits.s2, pc, pointer); edge()
      zero(dut.io.bpu.resp.bits.s2); prediction(dut.io.bpu.resp.bits.s3, pc, pointer); edge()
      zero(dut.io.bpu.resp.bits.s3)
      until("FTQ fetch request")(fetches.contains(pointer))
      assert(fetches(pointer) == pc)
      zero(dut.io.predecode.bits)
      setPtr(dut.io.predecode.bits.ftqIdx, pointer)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (field, i) => field.poke((pc + 2 * i).U) }
      offsets.zip(instructions).foreach { case (offset, instruction) =>
        dut.io.predecode.bits.pd(offset).valid.poke(true.B)
        dut.io.predecode.bits.pd(offset).isRVC.poke((instruction.bytes == 2).B)
        dut.io.predecode.bits.pd(offset).brType.poke(if ((instruction.bits & 0x7f) == 0x63)
          xiangshan.frontend.BrType.branch else xiangshan.frontend.BrType.notCFI)
        dut.io.predecode.bits.instrRange(offset).poke(true.B)
      }
      dut.io.predecode.bits.ftqOffset.poke(offsets.last.U)
      dut.io.predecode.bits.target.poke((pcs.last + instructions.last.bytes).U)
      dut.io.predecode.valid.poke(true.B); edge(); dut.io.predecode.valid.poke(false.B)
      var consumed = 0
      var waited = 0
      while (consumed < planned.size && waited < 2000) {
        lanes.zipWithIndex.foreach { case (lane, slot) =>
          val index = consumed + slot
          lane.valid.poke((index < planned.size).B)
          zero(lane.bits)
          if (index < planned.size) {
            val source = planned(index)
            lane.bits.instr.poke(source.instruction.bits.U)
            lane.bits.pc.poke(source.pc.U)
            lane.bits.fdiNotTrusted.foreach(_.poke(source.instruction.tag.B))
            lane.bits.trigger.poke(xiangshan.TriggerAction.None)
            lane.bits.pd.valid.poke(true.B)
            lane.bits.pd.isRVC.poke((source.instruction.bytes == 2).B)
            lane.bits.pd.brType.poke(if ((source.instruction.bits & 0x7f) == 0x63)
              xiangshan.frontend.BrType.branch else xiangshan.frontend.BrType.notCFI)
            setPtr(lane.bits.ftqPtr, pointer)
            lane.bits.ftqOffset.poke(offsets(index).U)
            lane.bits.isLastInFtqEntry.poke((index == planned.size - 1).B)
          }
        }
        val available = planned.size - consumed
        val accepted = (0 until available).takeWhile(i => bool(lanes(i).ready)).size
        assert((0 until available).count(i => bool(lanes(i).ready)) == accepted, "Frontend must accept a prefix")
        planned.slice(consumed, consumed + accepted).zipWithIndex.foreach { case (source, i) =>
          source.accepted = cycles
          record("frontend-accept", source, s"\"ftq_flag\":${pointer.flag},\"ftq\":${pointer.value},\"offset\":${offsets(consumed + i)}")
        }
        edge(); consumed += accepted; waited += 1
      }
      assert(consumed == planned.size, "Packet acceptance timed out")
      // Exercise a changed live input after acceptance while older uops proceed.
      lanes.foreach { lane => lane.valid.poke(false.B); zero(lane.bits); lane.bits.fdiNotTrusted.foreach(_.poke(true.B)) }
      planned
    }
    def execute(pc: BigInt, instruction: Instruction): Source = {
      val source = packet(pc, Seq(instruction)).head
      until("source retirement")(source.retired)
      idle(12)
      source
    }
    def scalar(): Unit = {
      val members = packet(0x1000, Seq(Instruction(addi(1, 0, 9), false),
        Instruction(addi(2, 0, 7), true), Instruction(register(3, 1, 2, funct7 = 1), false),
        Instruction(register(4, 1, 2, funct7 = 1), true), Instruction(addi(5, 3, 2), true),
        Instruction(addi(6, 4, 3), false)))
      until("scalar and multiply retirement")(members.forall(_.retired))
      idle(8)
      members.foreach { source =>
        assert(source.renamed == Set(0) && source.fuCount == 1)
        assert(source.enqueued.nonEmpty && source.issues.nonEmpty, "A scalar source skipped IQ coverage")
      }
      val multiplications = Seq(members(2), members(3))
      assert(multiplications.map(_.instruction.tag).toSet == Set(false, true))
      multiplications.foreach { source =>
        assert(source.keys.size == 1 && source.fuUnits.size == 1)
        val sourceKey = source.keys.head
        val selectedUnit = source.fuUnits.head
        val unit = dut.units(selectedUnit)
        assert(unit.cfg == FuConfig.MulCfg, "MUL was not accepted by the actual multiplier FU")
        val terminal = unit.io.in.bits.ctrlPipe.get.length - 1
        assert(terminal > 0, "The selected multiplier has no delayed control stage")
        for (stage <- 1 to terminal) {
          assert(source.pipeStages.contains((selectedUnit, stage, sourceKey)),
            s"MUL source ${source.serial} did not reach selected unit $selectedUnit stage $stage")
        }
        record("multiply-selected-pipeline-complete", source,
          s"\"unit\":$selectedUnit,\"terminal_stage\":$terminal,\"rob\":${sourceKey.rob.value}," +
            s"\"rob_flag\":${sourceKey.rob.flag},\"pdest\":${sourceKey.pdest},\"operation\":${sourceKey.operation}")
      }
    }
    def fusionMatrix(): Unit = {
      // Integer RAT reset maps every logical register to zero. Real writes make
      // selecting the wrong neighboring operand observable in this matrix.
      execute(0x1c00, Instruction(addi(10, 0, 9), false))
      execute(0x1d00, Instruction(addi(11, 0, 17), true))
      execute(0x1e00, Instruction(addi(5, 0, 13), false))
      var pc = BigInt(0x2000)
      for (selector <- Seq(1, 2, 4); baseTag <- Seq(false, true); mixed <- Seq(false, true)) {
        val first = shift(5, 10, if (selector == 1) 32 else 1)
        val second = if (selector == 1) shift(5, 5, 32, right = true)
          else if (selector == 2) register(5, 11, 5) else register(5, 5, 11)
        val expectation = Fusion(pc, pc + 4, !enabled || !mixed, selector)
        fusion(pc) = expectation
        dut.io.fusionEnabled.expect(true.B)
        val pair = packet(pc, Seq(Instruction(first, baseTag), Instruction(second, baseTag ^ mixed), Instruction(addi(7, 0, 5), !baseTag)))
        until("fusion pair completion")(pair.head.retired && pair.last.retired)
        idle(8)
        assert(expectation.samples > 0, "Fusion decision was never observed")
        assert(pair.head.renamed == Set(0))
        if (expectation.allowed) assert(pair(1).renamed.isEmpty, "Fused second instruction was not cleared")
        else {
          assert(pair(1).renamed == Set(0), "Mixed-tag second instruction was lost")
          assert(pair.head.fuCount == 1 && pair(1).fuCount == 1)
        }
        pc += 0x100
      }
    }
    def compression(): Unit = {
      val members = packet(0x3000, Seq(Instruction(addi(5, 0, 3), false, 2),
        Instruction(addi(6, 0, 4), true), Instruction(addi(7, 0, 5), false)))
      until("compressed ROB group retirement")(members.forall(_.retired))
      members.foreach(source => assert(source.fuCount == 1))
      assert(members.take(2).map(_.keys.head.rob).distinct.size == 1,
        "The intended mixed-tag scalar ROB compression was not exercised")
    }
    def memoryRetry(): Unit = {
      dut.metadata.memoryReady.poke(false.B)
      val members = packet(0x4000, Seq(Instruction(BigInt("00003283", 16), false),
        Instruction(BigInt("00803303", 16), true)))
      until("natural memory timeout and IQ reissue")(members.forall(_.retries > 0))
      assert(members.forall(_.memorySamples >= 16), "Memory backpressure did not hold real issue payloads")
      dut.metadata.memoryReady.poke(true.B)
      until("memory issue acceptance")(members.forall(_.memoryFires >= 1))
      assert(members.forall(_.memoryFires == 1), "A load was accepted more than once")
      idle(8)
    }
    def vector(): Unit = {
      execute(0x5000, Instruction(addi(1, 0, 1), false))
      execute(0x5100, Instruction(shift(1, 1, 9), true))
      execute(0x5200, Instruction(BigInt("3000a073", 16), false)) // CSRRS mstatus, x1.
      execute(0x5300, Instruction(BigInt("00307157", 16), true)) // VSETVLI e8, m8.
      val vectorMove = (BigInt(0x17) << 26) | (BigInt(1) << 25) | (BigInt(3) << 15) |
        (BigInt(3) << 12) | (BigInt(8) << 7) | 0x57
      val members = packet(0x5400, Seq(Instruction(vectorMove, true), Instruction(addi(12, 0, 9), false)))
      until("all vector split uops and opposite-tag follower retirement")(members.forall(_.retired))
      val move = members.head
      // VMV.V.I uses VEC_VXV: one immediate-to-vector helper followed by LMUL
      // vector operations. The helper and vector lane zero share uopIdx zero.
      val helper = move.fuAccepts.filter(event => dut.units(event._1).cfg == FuConfig.I2vCfg)
      val vectorOps = move.fuAccepts.filter(event => dut.units(event._1).cfg == FuConfig.VialuCfg)
      assert(move.fuCount == 9 && helper.size == 1 && vectorOps.size == 8,
        "VMV.V.I e8,m8 requires exactly one I2V helper and eight vector operations")
      assert(move.keys.size == 9 && move.renamedTokens.size == 9)
      assert(move.fuAccepts.map(_._3).distinct.size == 9 && move.fuAccepts.map(_._3).toSet == move.keys.toSet,
        "Every renamed VMV.V.I transaction must reach a real FU exactly once")
      // immDup2Vec is 0b110 concatenated with e8=0; vmv.v.v uses opcode 26.
      assert(helper.head._2 == 0 && helper.head._3.operation == 24)
      assert(vectorOps.map(_._2).sorted.toSeq == (0 until 8) && vectorOps.forall(_._3.operation == 26))
      move.fuAccepts.foreach { case (_, index, id) =>
        assert(move.renamedTokens((index, id)) && move.enqueued.contains(id) && move.issues.contains(id),
          "A VMV.V.I helper or vector transaction lacks its own Rename/IQ association")
      }
      vectorOps.foreach { case (unit, _, id) =>
        val terminal = dut.units(unit).io.in.bits.ctrlPipe.get.length - 1
        assert(terminal > 0 && move.pipeStages((unit, terminal, id)),
          "A VMV.V.I vector transaction missed its selected FU terminal control stage")
      }
      record("vector-helper-and-lanes-complete", move, "\"helper_count\":1,\"vector_count\":8,\"unique_keys\":9")
      assert(members(1).fuCount == 1)
    }
    def amocas(): Unit = {
      val members = packet(0x6000, Seq(Instruction(amocasDInstruction, true), Instruction(addi(12, 0, 9), false)))
      until("AMOCAS split reaching real memory issue")(members.head.renamed == Set(0, 1) && members.head.memoryFires >= 3)
      assert(members.head.keys.size == 2, "AMOCAS uop association collapsed distinct operands")
      // AMOCAS.D has two store-data uops; only the odd uop also issues an address.
      assert(members.head.memoryFires == 3)
      val first = members.head.renamedTokens.collectFirst { case (0, id) => id }.get
      val second = members.head.renamedTokens.collectFirst { case (1, id) => id }.get
      assert(first.operation == 111 && second.operation == 47)
      assert(members.head.memoryAccepts.toSet == Set(("store-data", first), ("store-data", second), ("address", second)))
      // The memory service deliberately stops at acceptance; no AMO execution is claimed.
      idle(8)
    }
    def cancellation(): Unit = {
      execute(0x7000, Instruction(addi(1, 0, 97), false))
      execute(0x7100, Instruction(addi(2, 0, 3), true))
      val divide = register(10, 1, 2, funct3 = 4, funct7 = 1)
      val branch = BigInt("04000063", 16) | (BigInt(10) << 15) | (BigInt(10) << 20)
      val members = packet(0x7200, Seq(Instruction(divide, false), Instruction(branch, true),
        Instruction(register(20, 10, 2, funct3 = 4, funct7 = 1), false), Instruction(addi(21, 20, 1), true)))
      until("older real branch redirect")(redirects > 0)
      idle(40)
      val young = members.last
      assert(young.enqueued.nonEmpty, "The canceled dependent instruction never occupied a real IQ")
      assert(young.fuCount == 0 && young.issues.isEmpty && !young.retired,
        "A canceled dependent instruction escaped IQ cancellation")
      record("canceled-iq-source", young)
    }
  }

  private def fusionHold(path: Path)(implicit p: Parameters): Unit = {
    val workspace = new Workspace(path.resolve("fusion-hold-compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDITrustFusionHoldHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      val dut = elaborated.wrapped
      val events = mutable.ArrayBuffer.empty[String]
      var cycle = 0
      var heldSamples = 0
      def edge(): Unit = { dut.clock.step(); cycle += 1 }
      def check(selector: Int, baseTag: Boolean, mixed: Boolean, disabled: Boolean, kind: String): Unit = {
        val allowed = !disabled && (!enabled || !mixed)
        dut.io.fused.expect(allowed.B)
        dut.io.clearNext.expect(allowed.B)
        dut.io.info.expect((if (allowed) selector else 0).U)
        dut.io.lsrc2Valid.expect(allowed.B)
        if (allowed) dut.io.lsrc2.expect((if (selector == 1) 0 else 11).U)
        events += s"{\"cycle\":$cycle,\"event\":\"$kind\",\"selector\":$selector," +
          s"\"first_tag\":$baseTag,\"second_tag\":${baseTag ^ mixed}," +
          s"\"captured_mixed\":$mixed,\"captured_disabled\":$disabled,\"expected_fusion\":$allowed}"
      }
      try {
        for (selector <- Seq(1, 2, 4); baseTag <- Seq(false, true); initialMixed <- Seq(false, true); disabled <- Seq(false, true)) {
          dut.io.valid.poke(false.B); dut.io.ready.poke(false.B); dut.io.disabled.poke(false.B)
          dut.io.instructions.foreach(_.poke(0.U)); dut.io.tags.foreach(_.poke(false.B))
          dut.reset.poke(true.B); edge(); edge(); dut.reset.poke(false.B)
          dut.io.instructions(0).poke(shift(5, 10, if (selector == 1) 32 else 1).U)
          val second = if (selector == 1) shift(5, 5, 32, right = true)
            else if (selector == 2) register(5, 11, 5) else register(5, 5, 11)
          dut.io.instructions(1).poke(second.U)
          dut.io.tags(0).poke(baseTag.B)
          dut.io.tags(1).poke((baseTag ^ initialMixed).B)
          dut.io.disabled.poke(disabled.B)
          dut.io.valid.poke(true.B); dut.io.ready.poke(true.B); edge()
          check(selector, baseTag, initialMixed, disabled, "accepted-a")

          // A fired. B may now replace it, but B stays completely stable while
          // ready is low; the pending T1 decision must still belong to A.
          dut.io.tags(1).poke((baseTag ^ !initialMixed).B)
          dut.io.disabled.poke((!disabled).B)
          dut.io.ready.poke(false.B)
          for (_ <- 0 until 5) {
            check(selector, baseTag, initialMixed, disabled, "held-valid-b")
            heldSamples += 1
            edge()
          }
          dut.io.ready.poke(true.B); edge()
          check(selector, baseTag, !initialMixed, !disabled, "accepted-b")

          // B fired. With no next instruction, changing invalid payload bits
          // must not update the saved qualification when inReady is also low.
          dut.io.valid.poke(false.B); dut.io.ready.poke(false.B)
          for (tag <- Seq(false, true, false, true)) {
            dut.io.tags(0).poke(tag.B); dut.io.tags(1).poke((!tag).B)
            dut.io.disabled.poke(tag.B)
            edge()
            check(selector, baseTag, !initialMixed, !disabled, "invalid-next-payload")
          }
        }
        assert(heldSamples == 120)
        println(s"FDI fusion capture/hold PASS enabled=$enabled heldSamples=$heldSamples fixture=production-FusionDecoder")
      } finally {
        Files.write(path.resolve("fusion-hold-events.jsonl"), events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }

  it should "preserve independent source tags through real decode, issue and consumer control" in {
    val root = Paths.get(sys.props("c08.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated C08 evidence directory")
    val path = root.resolve(if (enabled) "backend-enabled" else "backend-disabled")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
    val options = p(xiangshan.DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    if (scenario == "full") fusionHold(path)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDITrustMetadataBackendHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      val driver = new Driver(elaborated.wrapped)
      try {
        driver.reset(); driver.scalar()
        if (scenario == "full") {
          driver.reset(); driver.fusionMatrix()
          driver.reset(); driver.compression()
          driver.reset(); driver.memoryRetry()
          driver.reset(); driver.vector()
          driver.reset(); driver.amocas()
          driver.reset(); driver.cancellation()
        }
        println(s"FDI metadata backend PASS scenario=$scenario enabled=$enabled cycles=${driver.cycles} " +
          s"sources=${driver.nextSerial} counts=${driver.totals.toSeq.sortBy(_._1).mkString(",")} " +
          "input=independent-CtrlFlow-tag backend=production ftq=production endpoint=FU-control-and-memory-issue")
      } finally {
        Files.write(path.resolve("metadata-events.jsonl"), driver.events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
