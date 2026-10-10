// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{BranchPredictionBundle, FtqPtr}

case class FDIControlPointer(flag: Boolean, value: Int)
case class FDIControlFetch(pointer: FDIControlPointer, offset: Int)
case class FDIControlInstruction(bits: BigInt, tag: Boolean = false, bytes: Int = 4,
  branchType: Int = 0, predictedTaken: Boolean = false, predictedTarget: Option[BigInt] = None,
  fetchException: Int = 0) {
  require(bytes == 2 || bytes == 4)
}
case class FDIControlOutput(cycle: Int, kind: String, rfWen: Boolean, data: BigInt,
  target: BigInt, exception: BigInt, redirect: Boolean, mispredicted: Boolean,
  tag: Boolean, callEvent: Boolean)
case class FDIControlTrap(cycle: Int, cause: BigInt, epc: BigInt, tval: BigInt, reason: Int)
case class FDIControlInterruptAcceptance(cycle: Int, source: Int, rob: FDIControlPointer,
  fetch: FDIControlFetch, isRVC: Boolean, cause: Int, debug: Boolean, criticalDebug: Boolean,
  nmi: Boolean, hu: Boolean, hs: Boolean, vs: Boolean, isInterrupt: Boolean)
case class FDIControlEntry(cycle: Int, kind: String, accepted: Option[FDIControlInterruptAcceptance],
  cause: BigInt, epc: BigInt, tval: BigInt, reason: Int)
case class FDIControlCSRResult(cycle: Int, pdest: Int, exception: BigInt)
case class FDIControlExceptionIngress(cycle: Int, port: Int, exception: BigInt, eligible: Boolean, raw: Option[BigInt])
class FDIControlSource(val id: Int, val pc: BigInt, val instruction: FDIControlInstruction,
  val fetch: FDIControlFetch) {
  var accepted = -1
  var allocated = -1
  var retired = -1
  var canceled = -1
  var expanded = BigInt(0)
  var rob: Option[FDIControlPointer] = None
  var pdest = -1
  var waitForward = false
  var blockBackward = false
  val issues = mutable.ArrayBuffer.empty[Int]
  val outputs = mutable.ArrayBuffer.empty[FDIControlOutput]
  val writes = mutable.ArrayBuffer.empty[BigInt]
  val writeCycles = mutable.ArrayBuffer.empty[Int]
  val writebacks = mutable.ArrayBuffer.empty[Int]
  val callCycles = mutable.ArrayBuffer.empty[Int]
  val csrRequests = mutable.ArrayBuffer.empty[Int]
  val csrWriteRequests = mutable.ArrayBuffer.empty[Int]
  val csrResults = mutable.ArrayBuffer.empty[FDIControlCSRResult]
  val exceptionIngress = mutable.ArrayBuffer.empty[FDIControlExceptionIngress]
  var trap: Option[FDIControlTrap] = None
}

// Expectations are formed by callers from encoded instructions and initial
// state. This driver records production events without computing permissions.
class FDIFullControlDriver(val dut: FDIFullControlHarness, val path: Path) {
  val enabled: Boolean = dut.HasFDI
  var cycle = 0
  var helperPc = BigInt(0x1000)
  var caseName = "setup"
  val sources = mutable.ArrayBuffer.empty[FDIControlSource]
  val byFetch = mutable.Map.empty[FDIControlFetch, FDIControlSource]
  val byRob = mutable.Map.empty[FDIControlPointer, FDIControlSource]
  val byPhysical = mutable.Map.empty[Int, FDIControlSource]
  val fetched = mutable.Set.empty[FDIControlPointer]
  val frontendRedirects = mutable.ArrayBuffer.empty[(Int, BigInt)]
  val eventCounts = mutable.Map.empty[String, Int].withDefaultValue(0)
  val interruptAcceptances = mutable.ArrayBuffer.empty[FDIControlInterruptAcceptance]
  val entries = mutable.ArrayBuffer.empty[FDIControlEntry]
  private val log = Files.newBufferedWriter(path.resolve("events.jsonl"), StandardCharsets.UTF_8)
  private val lanes = dut.io.instruction +: dut.io.instructionTail.toSeq
  private var trapOwner: Option[FDIControlSource] = None
  private var nextCall: Option[(FDIControlSource, BigInt)] = None
  // Retain the actual accepted identity through the CSR effect. This is a
  // passive test ledger; no token is injected into the processor.
  private var acceptedInterrupt: Option[FDIControlInterruptAcceptance] = None
  def bool(x: Bool): Boolean = x.peek().litToBoolean
  def uint(x: UInt): BigInt = x.peek().litValue
  def ptr(x: RobPtr): FDIControlPointer = FDIControlPointer(bool(x.flag), uint(x.value).toInt)
  def ptr(x: FtqPtr): FDIControlPointer = FDIControlPointer(bool(x.flag), uint(x.value).toInt)
  def fetch(x: FtqPtr, offset: UInt): FDIControlFetch = FDIControlFetch(ptr(x), uint(offset).toInt)
  def poke(x: UInt, value: BigInt): Unit = x.poke((value & ((BigInt(1) << x.getWidth) - 1)).U)
  def zero(x: Data): Unit = x match {
    case b: Bool => b.poke(false.B)
    case u: UInt => u.poke(0.U)
    case s: SInt => s.poke(0.S)
    case v: Vec[_] => v.foreach(zero)
    case r: Record => r.elements.values.foreach(zero)
    case other => throw new IllegalArgumentException(s"Unsupported test input ${other.getClass.getName}")
  }
  def setPtr(x: FtqPtr, p: FDIControlPointer): Unit = {
    x.flag.poke(p.flag.B); x.value.poke(p.value.U)
  }
  def record(kind: String, fields: (String, Json)*): Unit = {
    eventCounts(kind) += 1
    val json = Json.fromFields(Seq("cycle" -> Json.fromInt(cycle), "case" -> Json.fromString(caseName),
      "event" -> Json.fromString(kind)) ++ fields)
    log.write(json.noSpaces); log.newLine()
  }
  private def sourceEvent(kind: String, source: FDIControlSource, fields: (String, Json)*): Unit =
    record(kind, (Seq("source" -> Json.fromInt(source.id), "pc" -> Json.fromBigInt(source.pc)) ++ fields): _*)
  def owner(address: Int): BigInt = uint(dut.control.owner(FDIFullControlAddresses.owner.indexOf(address)))
  def mirror(address: Int): BigInt = uint(dut.control.mirror(FDIFullControlAddresses.mirror.indexOf(address)))
  def expectSpecial(address: Int, value: BigInt): Unit = {
    assert(owner(address) == value, s"CSR 0x${address.toHexString}: expected $value, got ${owner(address)}")
    if (FDIFullControlAddresses.mirror.contains(address)) assert(mirror(address) == value)
  }
  def edge(): Unit = {
    nextCall.foreach { case (source, value) =>
      expectSpecial(0x8b1, value)
      sourceEvent("call-owner-mirror-visible", source, "value" -> Json.fromBigInt(value))
    }
    nextCall = None
    if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady)) fetched += ptr(dut.io.fetchRequest.bits.ftqIdx)
    dut.control.renamed.foreach { port =>
      if (bool(port.valid)) {
        val in = port.bits
        val source = byFetch.getOrElse(fetch(in.ftqIdx, in.ftqOffset),
          throw new AssertionError(s"Unplanned rename at $cycle"))
        assert(uint(in.instr) == source.expanded, "Rename changed the expanded instruction")
        assert(bool(in.tag) == (enabled && source.instruction.tag), "Rename changed the original source tag")
        source.pdest = uint(in.pdest).toInt
        source.waitForward = bool(in.waitForward); source.blockBackward = bool(in.blockBackward)
        if (bool(in.rfWen)) byPhysical(source.pdest) = source
        sourceEvent("rename", source, "expanded" -> Json.fromBigInt(source.expanded),
          "pdest" -> Json.fromInt(source.pdest), "wait_forward" -> Json.fromBoolean(source.waitForward),
          "block_backward" -> Json.fromBoolean(source.blockBackward))
      }
    }
    dut.io.robEnq.foreach { port =>
      if (bool(port.valid) && bool(port.bits.first) && !bool(dut.control.redirect.valid)) {
        val source = byFetch.getOrElse(fetch(port.bits.ftqIdx, port.bits.ftqOffset),
          throw new AssertionError("ROB allocation lost frontend identity"))
        assert(source.allocated < 0, s"Duplicate ROB allocation for ${source.id}")
        source.allocated = cycle; source.rob = Some(ptr(port.bits.robIdx)); byRob(source.rob.get) = source
        sourceEvent("allocate", source, "rob" -> Json.fromInt(source.rob.get.value),
          "rob_flag" -> Json.fromBoolean(source.rob.get.flag))
      }
    }
    if (bool(dut.control.redirect.valid)) {
      val r = dut.control.redirect.bits
      val anchor = ptr(r.robIdx)
      val size = dut.RobSize
      val point = anchor.value + (if (anchor.flag) size else 0)
      val self = uint(r.level) == 1
      sources.filter(s => s.allocated >= 0 && s.retired < 0 && s.canceled < 0).foreach { source =>
        val p = source.rob.get
        val distance = (p.value + (if (p.flag) size else 0) - point + 2 * size) % (2 * size)
        if ((distance > 0 && distance < size) || (distance == 0 && self)) {
          source.canceled = cycle; sourceEvent("cancel", source)
        }
      }
      // Every accepted but unallocated frontend item is younger than a live
      // ROB redirect owner and is discarded by the native decode recovery.
      sources.filter(s => s.accepted >= 0 && s.allocated < 0 && s.canceled < 0).foreach { source =>
        source.canceled = cycle; sourceEvent("frontend-cancel", source)
      }
      record("redirect", "target" -> Json.fromBigInt(uint(r.fullTarget)), "self" -> Json.fromBoolean(self))
    }
    dut.control.execution.zipWithIndex.foreach { case (port, index) =>
      if (bool(port.inFire) || bool(port.outFire)) {
        val source = byRob.getOrElse(ptr(port.robIdx), throw new AssertionError("FU lost ROB identity"))
        assert(source.fetch == fetch(port.ftqIdx, port.ftqOffset), "FU changed FTQ identity")
        assert(bool(port.tag) == (enabled && source.instruction.tag), "FU changed source tag")
        if (bool(port.inFire)) source.issues += cycle
        if (bool(port.outFire)) {
          // Control announces recovery one cycle before the EXU receives its
          // age-filtered flush. A same-edge speculative ordinary result is
          // legal; an implicit call effect or any later result is not.
          assert(source.canceled < 0 || (source.canceled == cycle && !bool(port.call.valid)),
            "A canceled control instruction completed after its flush boundary")
          val out = FDIControlOutput(cycle, dut.controlKinds(index), bool(port.rfWen), uint(port.data),
            uint(port.redirect.bits.fullTarget), uint(port.exception), bool(port.redirect.valid),
            bool(port.redirect.bits.cfiUpdate.isMisPred), bool(port.tag), bool(port.call.valid))
          source.outputs += out
          sourceEvent("fu-complete", source, "kind" -> Json.fromString(out.kind),
            "exception" -> Json.fromBigInt(out.exception), "target" -> Json.fromBigInt(out.target),
            "rf_wen" -> Json.fromBoolean(out.rfWen), "data" -> Json.fromBigInt(out.data),
            "redirect" -> Json.fromBoolean(out.redirect), "mispredicted" -> Json.fromBoolean(out.mispredicted))
          if (out.callEvent) {
            assert(bool(dut.control.waitForward) && bool(dut.control.blockBackward),
              "Implicit ReturnPC update requires the real serial owner")
            assert(bool(dut.control.call.valid) && uint(dut.control.call.bits) == out.data)
            assert(bool(dut.control.distribution.w.valid) && uint(dut.control.distribution.w.bits.addr) == 0x8b1)
            assert(uint(dut.control.distribution.w.bits.data) == out.data)
            assert(nextCall.isEmpty, "Multiple call effects in one cycle")
            source.callCycles += cycle; nextCall = Some(source -> out.data)
            sourceEvent("call-effect", source, "value" -> Json.fromBigInt(out.data))
          }
        }
      }
    }
    dut.control.rf.foreach { port =>
      if (bool(port.valid) && uint(port.pdest) != 0) {
        val source = byPhysical.getOrElse(uint(port.pdest).toInt,
          throw new AssertionError(s"RF write lacks a renamed destination at $cycle"))
        source.writes += uint(port.data); source.writeCycles += cycle
        sourceEvent("physical-register-write", source, "value" -> Json.fromBigInt(uint(port.data)))
      }
    }
    dut.io.scalarWriteback.foreach { port =>
      if (bool(port.valid)) byRob.get(ptr(port.bits)).foreach { source =>
        source.writebacks += cycle; sourceEvent("writeback", source)
      }
    }
    if (bool(dut.io.csrRequest.valid)) {
      val source = byRob.getOrElse(ptr(dut.io.csrRequestRob),
        throw new AssertionError("A real CSR request lost its ROB owner"))
      source.csrRequests += cycle
      val writes = bool(dut.control.csrWriteRequest)
      if (writes) source.csrWriteRequests += cycle
      sourceEvent("csr-request", source, "address" -> Json.fromBigInt(uint(dut.io.csrRequest.bits)),
        "write_request" -> Json.fromBoolean(writes))
    }
    if (bool(dut.control.csrResult.valid)) {
      val value = dut.control.csrResult.bits
      val source = byRob.getOrElse(ptr(value.robIdx), throw new AssertionError("CSR response lost its original ROB owner"))
      val result = FDIControlCSRResult(cycle, uint(value.pdest).toInt, uint(value.exception))
      assert(result.pdest == source.pdest, "CSR response changed its physical destination identity")
      source.csrResults += result
      sourceEvent("csr-result", source, "pdest" -> Json.fromInt(result.pdest),
        "exception" -> Json.fromBigInt(result.exception))
    }
    dut.control.exceptionIngress.zipWithIndex.foreach { case (port, index) =>
      if (bool(port.valid)) {
        val value = port.bits
        val source = byRob.getOrElse(ptr(value.robIdx), throw new AssertionError("Exception writeback lost its original ROB owner"))
        // A raw word has meaning only on a valid raw payload, so avoid both
        // comparing and recording inactive payload bits.
        val raw = if (bool(value.raw.valid)) Some(uint(value.raw.bits)) else None
        val arrival = FDIControlExceptionIngress(cycle, index, uint(value.exception), bool(value.eligible), raw)
        source.exceptionIngress += arrival
        sourceEvent("exception-ingress", source, "port" -> Json.fromInt(index),
          "exception" -> Json.fromBigInt(arrival.exception), "eligible" -> Json.fromBoolean(arrival.eligible),
          "raw_valid" -> Json.fromBoolean(raw.nonEmpty), "raw" -> raw.map(Json.fromBigInt).getOrElse(Json.Null))
      }
    }
    dut.io.robRetire.foreach { port =>
      if (bool(port.valid)) {
        val source = byRob.getOrElse(ptr(port.bits), throw new AssertionError("Retirement lost original owner"))
        assert(source.retired < 0 && source.canceled < 0, "A retired or canceled owner retired again")
        source.retired = cycle; sourceEvent("retire", source)
      }
    }
    if (bool(dut.io.robException.valid)) {
      val pc = uint(dut.io.robException.bits.pc)
      val mask = (BigInt(1) << dut.io.robException.bits.pc.getWidth) - 1
      trapOwner = sources.reverse.find(s => s.allocated >= 0 && s.retired < 0 && (s.pc & mask) == pc)
      record("rob-exception", "pc" -> Json.fromBigInt(pc),
        "interrupt" -> Json.fromBoolean(bool(dut.io.robException.bits.isInterrupt)))
    }
    if (bool(dut.io.accepted.valid)) {
      val identity = dut.io.accepted.bits
      val token = ptr(identity.robIdx)
      val source = byRob.getOrElse(token, throw new AssertionError("Interrupt acceptance lost its ROB owner"))
      val location = fetch(identity.ftqIdx, identity.ftqOffset)
      assert(location == source.fetch, "Interrupt acceptance changed the owner's FTQ identity")
      val descriptor = identity.interrupt
      val accepted = FDIControlInterruptAcceptance(cycle, source.id, token, location, bool(identity.isRVC),
        uint(descriptor.cause).toInt, bool(descriptor.debug), bool(descriptor.criticalDebug),
        bool(descriptor.nmi), bool(descriptor.irToHU), bool(descriptor.irToHS), bool(descriptor.irToVS),
        bool(descriptor.isInterrupt))
      interruptAcceptances += accepted; acceptedInterrupt = Some(accepted)
      sourceEvent("interrupt-accepted", source,
        "rob" -> Json.fromInt(token.value), "rob_flag" -> Json.fromBoolean(token.flag),
        "ftq" -> Json.fromInt(location.pointer.value), "ftq_flag" -> Json.fromBoolean(location.pointer.flag),
        "offset" -> Json.fromInt(location.offset), "is_rvc" -> Json.fromBoolean(accepted.isRVC),
        "cause" -> Json.fromInt(accepted.cause), "debug" -> Json.fromBoolean(accepted.debug),
        "critical_debug" -> Json.fromBoolean(accepted.criticalDebug), "nmi" -> Json.fromBoolean(accepted.nmi),
        "to_hu" -> Json.fromBoolean(accepted.hu), "to_hs" -> Json.fromBoolean(accepted.hs),
        "to_vs" -> Json.fromBoolean(accepted.vs), "is_interrupt" -> Json.fromBoolean(accepted.isInterrupt),
        "wait_forward" -> Json.fromBoolean(bool(dut.control.waitForward)))
    }
    if (bool(dut.io.frontendRedirect.valid)) {
      val target = uint(dut.io.frontendRedirect.bits.cfiUpdate.target)
      frontendRedirects += cycle -> target
      record("frontend-redirect", "target" -> Json.fromBigInt(target))
    }
    val mTrap = bool(dut.io.mEntry)
    val sTrap = bool(dut.io.hsEntry)
    val nmi = bool(dut.io.mnEntry)
    val debug = bool(dut.io.debugEntry)
    val hu = bool(dut.io.entryEffect)
    if (nmi) record("nmi-entry")
    if (debug) record("debug-entry")
    if (hu) record("hu-entry")
    dut.clock.step(); cycle += 1
    val entryKind = if (mTrap) Some("m") else if (sTrap) Some("hs")
      else if (nmi) Some("nmi") else if (debug) Some("debug") else if (hu) Some("hu") else None
    assert(Seq(mTrap, sTrap, nmi, debug, hu).count(value => value) <= 1,
      "One precise source cannot produce different architectural entry kinds on the same edge")
    entryKind.foreach { kind =>
      val (cause, epc, tval) = kind match {
        case "m" => (uint(dut.io.mcause), uint(dut.io.mepc), uint(dut.io.mtval))
        case "hs" => (uint(dut.io.scause), uint(dut.io.sepc), uint(dut.control.stval))
        case "nmi" => (uint(dut.io.mncause), uint(dut.io.mnepc), BigInt(0))
        case "debug" => ((uint(dut.io.dcsr) >> 6) & 7, uint(dut.io.dpc), BigInt(0))
        case "hu" => (uint(dut.io.ucause), uint(dut.io.uepc), uint(dut.io.utval))
      }
      val entry = FDIControlEntry(cycle, kind, acceptedInterrupt, cause, epc, tval,
        if (enabled) owner(0x8b3).toInt else 0)
      entries += entry
      record("architectural-entry", "kind" -> Json.fromString(kind),
        "accepted_source" -> acceptedInterrupt.map(x => Json.fromInt(x.source)).getOrElse(Json.Null),
        "accepted_cycle" -> acceptedInterrupt.map(x => Json.fromInt(x.cycle)).getOrElse(Json.Null),
        "cause" -> Json.fromBigInt(cause), "epc" -> Json.fromBigInt(epc),
        "tval" -> Json.fromBigInt(tval), "reason" -> Json.fromInt(entry.reason))
      acceptedInterrupt = None
    }
    if (mTrap || sTrap) {
      val value = FDIControlTrap(cycle, uint(if (mTrap) dut.io.mcause else dut.io.scause),
        uint(if (mTrap) dut.io.mepc else dut.io.sepc), uint(if (mTrap) dut.io.mtval else dut.control.stval),
        if (enabled) owner(0x8b3).toInt else 0)
      trapOwner.foreach { source => source.trap = Some(value); sourceEvent("trap", source,
        "cause" -> Json.fromBigInt(value.cause), "epc" -> Json.fromBigInt(value.epc),
        "tval" -> Json.fromBigInt(value.tval), "reason" -> Json.fromInt(value.reason)) }
      trapOwner = None
    }
  }
  def idle(n: Int): Unit = (0 until n).foreach(_ => edge())
  def until(label: String, maximum: Int = 3000)(condition: => Boolean): Unit = {
    var n = 0
    while (!condition && n < maximum) { edge(); n += 1 }
    assert(condition, s"Timed out waiting for $label at $cycle ($caseName)")
  }
  def reset(): Unit = {
    lanes.foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
    dut.io.bpu.resp.valid.poke(false.B); zero(dut.io.bpu.resp.bits)
    zero(dut.io.predecode); zero(dut.io.interrupts)
    dut.io.fetchReady.poke(true.B); dut.io.traceEnable.poke(false.B); dut.io.traceStall.poke(false.B)
    dut.reset.poke(true.B); dut.clock.step(10); dut.reset.poke(false.B)
    byFetch.clear(); byRob.clear(); byPhysical.clear(); fetched.clear(); trapOwner = None; nextCall = None
    acceptedInterrupt = None
    sources.foreach(source => if (source.retired < 0 && source.canceled < 0) source.canceled = cycle)
    helperPc = 0x1000
    idle(400); dut.io.mode.expect(3.U); dut.io.virtualMode.expect(false.B)
    record("reset")
  }
  private def callReturnFlags(instruction: FDIControlInstruction): (Boolean, Boolean) = {
    val word = instruction.bits
    val compressed = instruction.bytes == 2
    val rd = if (compressed) {
      if (instruction.branchType == 3 && word.testBit(12)) 1 else 0
    } else if (instruction.branchType == 2 && (word & 0x7f) == 0x0b) 1
    else ((word >> 7) & 31).toInt
    val rs = if (compressed) ((word >> 7) & 31).toInt else ((word >> 15) & 31).toInt
    val isCall = (instruction.branchType == 2 || instruction.branchType == 3) && Set(1, 5)(rd)
    val isReturn = instruction.branchType == 3 && Set(1, 5)(rs) && !isCall
    isCall -> isReturn
  }
  private def prediction(stage: BranchPredictionBundle, base: BigInt, pointer: FDIControlPointer,
    planned: Seq[FDIControlSource]): Unit = {
    zero(stage); stage.pc.foreach(poke(_, base)); stage.valid.foreach(_.poke(true.B)); setPtr(stage.ftq_idx, pointer)
    planned.find(_.instruction.predictedTaken).foreach { source =>
      stage.full_pred.foreach { pred =>
        pred.hit.poke(true.B); pred.slot_valids.last.poke(true.B)
        poke(pred.targets.last, source.instruction.predictedTarget.get)
        poke(pred.jalr_target, source.instruction.predictedTarget.get)
        pred.offsets.last.poke(source.fetch.offset.U)
        pred.is_jal.poke((source.instruction.branchType == 2).B)
        pred.is_jalr.poke((source.instruction.branchType == 3).B)
        pred.is_br_sharing.poke((source.instruction.branchType == 1).B)
        pred.br_taken_mask.last.poke((source.instruction.branchType == 1).B)
        pred.is_call.poke(callReturnFlags(source.instruction)._1.B)
        pred.is_ret.poke(callReturnFlags(source.instruction)._2.B)
        poke(pred.fallThroughAddr, source.pc + source.instruction.bytes)
      }
    }
  }
  private def directTarget(source: FDIControlSource): BigInt = {
    val word = source.instruction.bits
    def bit(i: Int): BigInt = (word >> i) & 1
    val (imm, width) = if (source.instruction.bytes == 2) {
      ((bit(12) << 11) | (bit(11) << 4) | (((word >> 9) & 3) << 8) |
        (bit(8) << 10) | (bit(7) << 6) | (bit(6) << 7) | (((word >> 3) & 7) << 1) | (bit(2) << 5), 12)
    } else if ((word & 0x7f) == 0x0b) {
      ((bit(31) << 22) | (bit(7) << 21) | (((word >> 15) & 63) << 15) |
        (((word >> 8) & 15) << 11) | (((word >> 21) & 1023) << 1), 23)
    } else {
      ((bit(31) << 20) | (((word >> 21) & 1023) << 1) | (bit(20) << 11) | (((word >> 12) & 255) << 12), 21)
    }
    val signed = if (imm.testBit(width - 1)) imm - (BigInt(1) << width) else imm
    (source.pc + signed) & ((BigInt(1) << 64) - 1)
  }
  def issueBatch(pc: BigInt, instructions: Seq[FDIControlInstruction]): Seq[FDIControlSource] = {
    require(instructions.nonEmpty && instructions.size <= lanes.size)
    until("old frontend redirect drains")(!bool(dut.io.backendRedirect.valid) && !bool(dut.io.frontendRedirect.valid))
    val pointer = ptr(dut.io.ftqNext)
    val base = pc - pc % (dut.io.predecode.bits.pd.length * 2)
    var at = pc
    val planned = instructions.map { instruction =>
      val id = FDIControlFetch(pointer, ((at - base) / 2).toInt)
      require(id.offset < dut.io.predecode.bits.pd.length)
      val source = new FDIControlSource(sources.size, at, instruction, id)
      sources += source; byFetch(id) = source; at += instruction.bytes; source
    }
    fetched -= pointer
    zero(dut.io.bpu.resp.bits); prediction(dut.io.bpu.resp.bits.s1, base, pointer, planned)
    planned.find(_.instruction.predictedTaken).foreach { source =>
      val entry = dut.io.bpu.resp.bits.last_stage_ftb_entry
      entry.valid.poke(true.B); entry.isCall.poke(callReturnFlags(source.instruction)._1.B)
      entry.isRet.poke(callReturnFlags(source.instruction)._2.B)
      entry.isJalr.poke((source.instruction.branchType == 3).B)
      entry.tailSlot.valid.poke(true.B); entry.tailSlot.offset.poke(source.fetch.offset.U)
      entry.tailSlot.sharing.poke((source.instruction.branchType == 1).B)
      poke(entry.tailSlot.lower, source.instruction.predictedTarget.get >> 1)
      entry.tailSlot.tarStat.poke(0.U)
      poke(entry.pftAddr, (source.pc + source.instruction.bytes) >> 1)
    }
    dut.io.bpu.resp.valid.poke(true.B); until("BPU ready")(bool(dut.io.bpu.resp.ready)); edge()
    dut.io.bpu.resp.valid.poke(false.B)
    zero(dut.io.bpu.resp.bits.s1); prediction(dut.io.bpu.resp.bits.s2, base, pointer, planned); edge()
    zero(dut.io.bpu.resp.bits.s2); prediction(dut.io.bpu.resp.bits.s3, base, pointer, planned); edge()
    zero(dut.io.bpu.resp.bits.s3); until("FTQ fetch")(fetched(pointer))
    zero(dut.io.predecode.bits); setPtr(dut.io.predecode.bits.ftqIdx, pointer)
    dut.io.predecode.bits.pc.zipWithIndex.foreach { case (value, i) => poke(value, base + 2 * i) }
    planned.foreach { source =>
      val pd = dut.io.predecode.bits.pd(source.fetch.offset)
      pd.valid.poke(true.B); pd.brType.poke(source.instruction.branchType.U)
      pd.isRVC.poke((source.instruction.bytes == 2).B)
      pd.isCall.poke(callReturnFlags(source.instruction)._1.B)
      pd.isRet.poke(callReturnFlags(source.instruction)._2.B)
      dut.io.predecode.bits.instrRange(source.fetch.offset).poke(true.B)
    }
    dut.io.predecode.bits.ftqOffset.poke(planned.last.fetch.offset.U)
    poke(dut.io.predecode.bits.target, planned.last.pc + planned.last.instruction.bytes)
    planned.find(_.instruction.branchType == 2).foreach { source =>
      dut.io.predecode.bits.cfiOffset.valid.poke(true.B)
      dut.io.predecode.bits.cfiOffset.bits.poke(source.fetch.offset.U)
      poke(dut.io.predecode.bits.jalTarget, directTarget(source))
    }
    dut.io.predecode.valid.poke(true.B); edge(); dut.io.predecode.valid.poke(false.B)
    var consumed = 0; var attempts = 0
    while (consumed < planned.size && attempts < 3000) {
      lanes.zipWithIndex.foreach { case (lane, slot) =>
        val index = consumed + slot
        zero(lane.bits); lane.valid.poke((index < planned.size).B)
        if (index < planned.size) {
          val source = planned(index); val instruction = source.instruction
          poke(lane.bits.instr, instruction.bits); poke(lane.bits.pc, source.pc)
          lane.bits.trigger.poke(TriggerAction.None); lane.bits.fdiNotTrusted.foreach(_.poke(instruction.tag.B))
          lane.bits.pd.valid.poke(true.B); lane.bits.pd.isRVC.poke((instruction.bytes == 2).B)
          lane.bits.pd.brType.poke(instruction.branchType.U)
          lane.bits.pd.isCall.poke(callReturnFlags(instruction)._1.B)
          lane.bits.pd.isRet.poke(callReturnFlags(instruction)._2.B)
          lane.bits.pred_taken.poke(instruction.predictedTaken.B)
          lane.bits.exceptionVec.zipWithIndex.foreach { case (bit, i) => bit.poke(((instruction.fetchException & (1 << i)) != 0).B) }
          setPtr(lane.bits.ftqPtr, pointer); lane.bits.ftqOffset.poke(source.fetch.offset.U)
          lane.bits.isLastInFtqEntry.poke((index == planned.size - 1).B)
        }
      }
      val offered = (planned.size - consumed).min(lanes.size)
      val accepted = (0 until offered).takeWhile(i => bool(lanes(i).ready)).size
      assert((0 until offered).count(i => bool(lanes(i).ready)) == accepted)
      (0 until accepted).foreach { i =>
        val source = planned(consumed + i)
        source.accepted = cycle; source.expanded = uint(dut.control.expanded(i))
        sourceEvent("frontend-accept", source, "raw" -> Json.fromBigInt(source.instruction.bits),
          "expanded" -> Json.fromBigInt(source.expanded), "mode" -> Json.fromBigInt(uint(dut.io.mode)),
          "virtual" -> Json.fromBoolean(bool(dut.io.virtualMode)), "tag" -> Json.fromBoolean(source.instruction.tag))
      }
      edge(); consumed += accepted; attempts += 1
    }
    lanes.foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
    assert(consumed == planned.size, "Frontend packet did not finish")
    planned
  }
  def issue(pc: BigInt, instruction: FDIControlInstruction): FDIControlSource = issueBatch(pc, Seq(instruction)).head
  def execute(pc: BigInt, instruction: FDIControlInstruction): FDIControlSource = {
    val source = issue(pc, instruction)
    until("retirement or precise exception")(source.retired >= 0 || source.trap.nonEmpty || source.canceled >= 0)
    idle(24); source
  }
  def expectRetired(source: FDIControlSource, expectedWrite: Option[BigInt] = None): Unit = {
    assert(source.retired >= 0 && source.canceled < 0 && source.trap.isEmpty, s"Source ${source.id} did not retire")
    expectedWrite.foreach(value => assert(source.writes.toSeq == Seq(value), s"Unexpected RF writes ${source.writes}"))
  }
  def expectNoWrite(source: FDIControlSource): Unit = assert(source.writes.isEmpty, s"Denied source wrote ${source.writes}")
  def expectTrap(source: FDIControlSource, cause: Int, tval: BigInt, reason: Int = 4): Unit = {
    val trap = source.trap.getOrElse(throw new AssertionError(s"Source ${source.id} had no trap"))
    assert(trap.cause == cause && trap.epc == source.pc && trap.tval == tval && trap.reason == reason,
      s"Expected cause=$cause epc=${source.pc} tval=$tval reason=$reason, got $trap")
    assert(source.retired < 0)
  }
  private def helper(bits: BigInt): FDIControlSource = {
    val pc = helperPc; helperPc += 4
    execute(pc, FDIControlInstruction(bits))
  }
  def setReg(rd: Int, value: BigInt): Unit = {
    require(rd > 0 && rd < 32)
    val normalized = value & ((BigInt(1) << 64) - 1)
    val bytes = (0 until 8).reverse.map(i => ((normalized >> (i * 8)) & 255).toInt).dropWhile(_ == 0)
    val values = if (bytes.isEmpty) Seq(0) else bytes
    expectRetired(helper((BigInt(values.head) << 20) | (BigInt(rd) << 7) | 0x13))
    values.tail.foreach { byte =>
      expectRetired(helper((BigInt(8) << 20) | (BigInt(rd) << 15) | (BigInt(1) << 12) | (BigInt(rd) << 7) | 0x13))
      if (byte != 0) expectRetired(helper((BigInt(byte) << 20) | (BigInt(rd) << 15) | (BigInt(rd) << 7) | 0x13))
    }
  }
  def writeCsr(address: Int, value: BigInt): Unit = {
    setReg(31, value)
    val source = helper((BigInt(address) << 20) | (BigInt(31) << 15) | 0x1073)
    expectRetired(source)
  }
  def readReg(rs: Int): BigInt = {
    // A nonzero immediate prevents rename move elimination, so the observed
    // production RF write is a real consumer of the architectural source.
    val source = helper((BigInt(1) << 20) | (BigInt(rs) << 15) | (BigInt(30) << 7) | 0x13)
    expectRetired(source)
    assert(source.writes.size == 1); (source.writes.head - 1) & ((BigInt(1) << 64) - 1)
  }
  def setup(mode: Int = 0, virtual: Boolean = false, mainCfg: BigInt = 2,
    trustedLo: BigInt = 0x1000, trustedHi: BigInt = 0x3000,
    csrs: Seq[(Int, BigInt)] = Seq.empty, registers: Seq[(Int, BigInt)] = Seq.empty): Unit = {
    reset()
    writeCsr(0x305, 0x8000); writeCsr(0x105, 0x9000)
    writeCsr(0x30c, 1); writeCsr(0x10c, 1)
    if (enabled) {
      writeCsr(0xbc5, trustedLo); writeCsr(0xbc6, trustedHi)
      writeCsr(0x9e2, trustedLo); writeCsr(0x9e3, trustedHi)
      writeCsr(0xbc4, mainCfg)
    }
    csrs.foreach { case (address, value) => writeCsr(address, value) }
    registers.foreach { case (register, value) => setReg(register, value) }
    if (mode != 3 || virtual) {
      writeCsr(0x300, (BigInt(mode) << 11) | (if (virtual) BigInt(1) << 39 else BigInt(0)))
      writeCsr(0x341, 0x1200)
      helper(BigInt("30200073", 16))
    }
    helperPc = 0x1200
    dut.io.mode.expect(mode.U); dut.io.virtualMode.expect(virtual.B)
    record("source-mode-ready", "mode" -> Json.fromInt(mode), "virtual" -> Json.fromBoolean(virtual))
  }
  def finish(error: Option[Throwable]): Unit = {
    log.flush(); log.close()
    val ledger = Json.arr(sources.map { source =>
      Json.obj("id" -> Json.fromInt(source.id), "pc" -> Json.fromBigInt(source.pc),
        "raw" -> Json.fromBigInt(source.instruction.bits), "expanded" -> Json.fromBigInt(source.expanded),
        "accepted" -> Json.fromInt(source.accepted), "allocated" -> Json.fromInt(source.allocated),
        "retired" -> Json.fromInt(source.retired), "canceled" -> Json.fromInt(source.canceled),
        "rob" -> source.rob.map(p => Json.obj("flag" -> Json.fromBoolean(p.flag), "value" -> Json.fromInt(p.value))).getOrElse(Json.Null),
        "ftq" -> Json.fromInt(source.fetch.pointer.value), "ftq_flag" -> Json.fromBoolean(source.fetch.pointer.flag),
        "ftq_offset" -> Json.fromInt(source.fetch.offset), "pdest" -> Json.fromInt(source.pdest),
        "issues" -> Json.arr(source.issues.map(Json.fromInt).toSeq: _*),
        "writeback_cycles" -> Json.arr(source.writebacks.map(Json.fromInt).toSeq: _*),
        "write_cycles" -> Json.arr(source.writeCycles.map(Json.fromInt).toSeq: _*),
        "write_values" -> Json.arr(source.writes.map(Json.fromBigInt).toSeq: _*),
        "call_cycles" -> Json.arr(source.callCycles.map(Json.fromInt).toSeq: _*),
        "csr_request_cycles" -> Json.arr(source.csrRequests.map(Json.fromInt).toSeq: _*),
        "csr_write_request_cycles" -> Json.arr(source.csrWriteRequests.map(Json.fromInt).toSeq: _*),
        "csr_results" -> Json.arr(source.csrResults.map(result => Json.obj(
          "cycle" -> Json.fromInt(result.cycle), "pdest" -> Json.fromInt(result.pdest),
          "exception" -> Json.fromBigInt(result.exception))).toSeq: _*),
        "exception_ingress" -> Json.arr(source.exceptionIngress.map(arrival => Json.obj(
          "cycle" -> Json.fromInt(arrival.cycle), "port" -> Json.fromInt(arrival.port),
          "exception" -> Json.fromBigInt(arrival.exception), "eligible" -> Json.fromBoolean(arrival.eligible),
          "raw" -> arrival.raw.map(Json.fromBigInt).getOrElse(Json.Null))).toSeq: _*))
    }.toSeq: _*)
    Files.write(path.resolve("sources.json"), (ledger.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
    val summary = Json.obj("has_fdi" -> Json.fromBoolean(enabled), "status" -> Json.fromString(if (error.isEmpty) "PASS" else "FAIL"),
      "cycles" -> Json.fromInt(cycle), "sources" -> Json.fromInt(sources.size),
      "events" -> Json.fromFields(eventCounts.toSeq.map { case (key, value) => key -> Json.fromInt(value) }),
      "error" -> error.map(x => Json.fromString(x.toString)).getOrElse(Json.Null),
      "scope" -> Json.fromString("Production Backend/FTQ with external frontend service; no whole-core DiffTest"))
    Files.write(path.resolve("result.json"), (summary.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
  }
}
