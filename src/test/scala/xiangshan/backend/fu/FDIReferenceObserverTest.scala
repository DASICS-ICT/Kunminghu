// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chiseltest._
import difftest.{FDIObservation, UserTimerRetire}
import chisel3.util.experimental.BoringUtils.{bore => observe}
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._

/** Inputs exercise the production wrapper; FDI state has no test write port. */
class FDIReferenceCSRHarness(implicit p: Parameters) extends FDICSRIntegrationHarness {
  val observation = IO(new Bundle {
    val allocate = Flipped(Valid(new UserTimerReferenceAllocation))
    val retire = Flipped(Valid(new UserTimerReferenceCommit))
    val boundary = Flipped(Valid(new UserTimerReferenceBoundary))
    val requestFlag = Input(Bool())
    val flushFlag = Input(Bool())
    val retired = Output(new UserTimerRetire)
    val retiredPc = Output(UInt(64.W))
    val retiredInstruction = Output(UInt(32.W))
    val packets = Output(Vec(4, new FDIObservation))
  })
  val identities = Module(new UserTimerReferenceObserver)
  def zeroInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zeroInputs)
    case vector: Vec[_] => vector.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  zeroInputs(identities.io)
  csr.io.in.bits.ctrl.robIdx.flag := observation.requestFlag
  csr.io.flush.bits.robIdx.flag := observation.flushFlag
  identities.io.enabled := coreParams.HasFDI.B
  identities.io.coreReset := reset.asBool
  identities.io.allocate(0) := observation.allocate
  identities.io.retire(0) := observation.retire
  identities.io.flush := csr.io.flush
  identities.io.softwareReady := true.B
  identities.io.boundary := observation.boundary
  val fdiObserver = FDIReferenceObservations.connect(csr, identities)
  observation.packets := fdiObserver.io.packets
  observation.retired := identities.io.committed(0)
  val observedPCs = observe(identities.pcs)
  val observedInstructions = observe(identities.instructions)
  observation.retiredPc := observedPCs(observation.retire.bits.ptr.value)
  observation.retiredInstruction := observedInstructions(observation.retire.bits.ptr.value)
}

/** Exercise the actual shared trap snapshot register with a source-mode change. */
class FDITrapPCObservationHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val allocate = Input(Bool())
    val boundary = Input(Bool())
    val trap = Input(Bool())
    val slot = Input(UInt(8.W))
    val pc = Input(UInt(64.W))
    val privilege = Input(UInt(2.W))
    val virtualMode = Input(Bool())
    val satpMode = Input(UInt(4.W))
    val vsatpMode = Input(UInt(4.W))
    val current = Output(UInt(64.W))
    val snapshotValid = Output(Bool())
    val snapshotPC = Output(UInt(64.W))
  })
  val shared = Module(new UserTimerReferenceObserver)
  def zeroInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zeroInputs)
    case vector: Vec[_] => vector.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  zeroInputs(shared.io)
  shared.io.enabled := true.B
  shared.io.coreReset := reset.asBool
  shared.io.softwareReady := true.B
  shared.io.allocate(0).valid := io.allocate
  shared.io.allocate(0).bits.ptr.value := io.slot
  shared.io.allocate(0).bits.pc := io.pc
  shared.io.allocate(0).bits.instr := "h00000073".U
  shared.io.boundary.valid := io.boundary
  shared.io.boundary.bits.ptr.value := io.slot
  shared.io.architecturalTrap := io.trap
  io.current := UserTimerReferenceTrapAddress(io.pc, io.privilege,
    io.virtualMode, io.satpMode, io.vsatpMode, 48)
  shared.io.trapPC := io.current
  io.snapshotValid := shared.io.snapshot.valid
  io.snapshotPC := shared.io.snapshot.pc
}

class FDIReferenceObserverTest extends AnyFlatSpec with ChiselScalatestTester {
  private def parameters(enabled: Boolean): Parameters = {
    val base = new top.DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
  }
  private def clear(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(clear)
    case value: Record => value.elements.values.foreach(clear)
    case _ => throw new IllegalArgumentException(s"Unsupported observation input $data")
  }
  // This literal oracle is independent of the producer's projection enumeration.
  private val projectionAddresses = Seq(
    0xbc4, 0x9e1, 0xbc5, 0xbc6, 0x9e2, 0x9e3, 0x880,
    0x890, 0x891, 0x892, 0x893, 0x894, 0x895, 0x896, 0x897,
    0x898, 0x899, 0x89a, 0x89b, 0x89c, 0x89d, 0x89e, 0x89f,
    0x8a0, 0x8a1, 0x8a2, 0x8a3, 0x8a4, 0x8a5, 0x8a6, 0x8a7,
    0x8a8, 0x8a9, 0x8aa, 0x8ab, 0x8ac, 0x8ad, 0x8ae, 0x8af,
    0x8b0, 0x8b1, 0x8b2, 0x8b3, 0x8c8,
    0x8c0, 0x8c1, 0x8c2, 0x8c3, 0x8c4, 0x8c5, 0x8c6, 0x8c7)
  private val allBits = (BigInt(1) << 64) - 1
  private def canonicalValue(address: Int, value: BigInt): BigInt = {
    val mask = address match {
      case 0xbc4 => BigInt(0x7ff)
      case 0x9e1 => BigInt(0x7c2)
      case 0x880 => BigInt("bbbbbbbbbbbbbbbb", 16)
      case 0x8b0 | 0x8b1 | 0x8b2 => allBits
      case 0x8b3 => BigInt(7)
      case 0x8c8 => BigInt("0001000100010001", 16)
      case _ => allBits ^ 7
    }
    value & mask
  }
  private def instruction(address: Int, write: Boolean): BigInt =
    (BigInt(address) << 20) | (if (write) 0x09173 else 0x02173)
  private def expectWords(packet: FDIObservation, expected: Map[Int, BigInt]): Unit = {
    projectionAddresses.zipWithIndex.foreach { case (address, index) =>
      packet.words(index).expect(expected.getOrElse(address, BigInt(0)).U)
    }
  }
  private def trace(packet: FDIObservation, lane: Int): Unit = {
    if (packet.valid.peek().litToBoolean) {
      val fields = packet.elements.toSeq.filterNot(_._1 == "words").map {
        case (name, value) => s"\"$name\":\"${value.asInstanceOf[UInt].peek().litValue}\""
      }
      val words = packet.words.map(word => s"\"${word.peek().litValue}\"").mkString("[", ",", "]")
      println(s"FDI_RTL_OBSERVATION {\"lane\":$lane,${fields.mkString(",")},\"words\":$words}")
    }
  }

  behavior of "FDI reference observations"

  it should "capture production reset and accepted CSR effects with shared identities despite response stalls" in {
    implicit val p: Parameters = parameters(true)
    test(new FDIReferenceCSRHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("fdi-production-observer"))) { dut =>
      clear(dut.io.request.bits)
      dut.io.request.valid.poke(false.B)
      dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.flushRob.poke(4.U)
      dut.io.trap.poke(false.B)
      dut.observation.requestFlag.poke(false.B)
      dut.observation.flushFlag.poke(false.B)
      clear(dut.observation.allocate)
      clear(dut.observation.retire)
      clear(dut.observation.boundary)
      dut.reset.poke(true.B)
      dut.clock.step(3)
      dut.reset.poke(false.B)
      def edge(): Unit = { dut.observation.packets.zipWithIndex.foreach { case (packet, lane) => trace(packet, lane) }; dut.clock.step() }
      val packets = dut.observation.packets
      packets(0).valid.expect(true.B)
      packets(0).kind.expect(1.U)
      expectWords(packets(0), Map.empty)
      edge()
      packets.foreach(_.valid.expect(false.B))
      val expected = scala.collection.mutable.Map.empty[Int, BigInt]
      var uid = 0
      val distinctPatterns = projectionAddresses.zipWithIndex.map { case (address, index) =>
        (address, true, BigInt("d123000000000000", 16) + (BigInt(index) << 16) + (address << 3) + 7, 0, false)
      }
      for ((address, write, value, stalls, cancel) <- Seq(
        (0x8b1, true, BigInt("f123456789abcdef", 16), 4, false),
        (0x8b1, false, BigInt(0), 1, false),
        (0xbc4, true, BigInt(0x7ff), 0, false),
        (0x9e1, true, BigInt(0x42), 2, false),
        (0x8b2, true, BigInt(0x4567), 0, true),
        (0x8b3, true, BigInt(7), 0, false)) ++ distinctPatterns) {
        uid += 1
        val inst = instruction(address, write)
        val flag = uid % 2 == 0
        dut.observation.requestFlag.poke(flag.B)
        dut.observation.flushFlag.poke(flag.B)
        val allocate = dut.observation.allocate
        allocate.valid.poke(true.B)
        allocate.bits.ptr.value.poke(4.U)
        allocate.bits.ptr.flag.poke(flag.B)
        allocate.bits.pc.poke(0x1016.U)
        allocate.bits.instr.poke(inst.U)
        edge()
        allocate.valid.poke(false.B)
        dut.io.request.ready.expect(true.B)
        dut.io.request.valid.poke(true.B)
        dut.io.request.bits.instruction.poke(inst.U)
        dut.io.request.bits.operation.poke((if (write) 9 else 10).U)
        dut.io.request.bits.operand.poke(value.U)
        dut.io.request.bits.rob.poke(4.U)
        dut.io.request.bits.basePc.poke(0x1000.U)
        dut.io.request.bits.offset.poke(11.U)
        dut.io.response.ready.poke((stalls == 0).B)
        packets(0).valid.expect(true.B)
        packets(0).uid.expect(uid.U)
        packets(0).pc.expect(0x1016.U)
        packets(0).instruction.expect(inst.U)
        packets(0).csrAddress.expect(address.U)
        packets(0).readNeeded.expect(true.B)
        packets(0).writeNeeded.expect(write.B)
        expectWords(packets(0), expected.toMap)
        edge()
        dut.io.request.valid.poke(false.B)
        // An offered younger address and source must not replace the accepted identity.
        dut.io.request.bits.instruction.poke(instruction(0x8b0, true).U)
        dut.io.request.bits.operand.poke(0x9999.U)
        dut.io.request.bits.rob.poke(7.U)
        dut.io.request.bits.basePc.poke(0x2000.U)
        dut.io.flush.poke(cancel.B)
        if (cancel) {
          packets(3).valid.expect(true.B)
          packets(3).uid.expect(uid.U)
          packets(3).cancelReason.expect(1.U)
          packets(3).actualWrite.expect(false.B)
          expectWords(packets(3), expected.toMap)
        }
        edge()
        dut.io.flush.poke(false.B)
        packets(1).valid.expect((!cancel).B)
        if (!cancel) {
          if (write) {
            if (address == 0xbc4) { expected(address) = value & 0x7ff; expected(0x9e1) = value & 0x7c2 }
            else if (address == 0x9e1) {
              expected(0xbc4) = (expected(0xbc4) & 0x3d) | (value & 0x7c2)
              expected(0x9e1) = value & 0x7c2
            } else expected(address) = canonicalValue(address, value)
          }
          packets(1).uid.expect(uid.U)
          packets(1).instruction.expect(inst.U)
          packets(1).pc.expect(0x1016.U)
          packets(1).actualWrite.expect(write.B)
          packets(1).responseIllegal.expect(false.B)
          expectWords(packets(1), expected.toMap)
        }
        edge()
        for (_ <- 0 until stalls) {
          packets.foreach(_.valid.expect(false.B))
          edge()
        }
        dut.io.response.ready.poke(true.B)
        edge()
        if (!cancel) {
          dut.observation.retire.valid.poke(true.B)
          dut.observation.retire.bits.ptr.value.poke(4.U)
          dut.observation.retire.bits.ptr.flag.poke(flag.B)
          dut.observation.retire.bits.count.poke(1.U)
          edge()
          dut.observation.retire.valid.poke(false.B)
        }
        edge()
      }
      println("FDI_PRODUCTION_OBSERVER_PASS source=wrapper.NewCSR identity=UserTimerReferenceObserver")
    }
  }

  it should "export a fixed production CSR program for independent reference execution" in {
    implicit val p: Parameters = parameters(true)
    test(new FDIReferenceCSRHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("fdi-production-reference-trace"))) { dut =>
      clear(dut.io.request.bits)
      dut.io.request.valid.poke(false.B); dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B); dut.io.flushRob.poke(4.U); dut.io.trap.poke(false.B)
      clear(dut.observation.allocate); clear(dut.observation.retire); clear(dut.observation.boundary)
      dut.observation.requestFlag.poke(false.B); dut.observation.flushFlag.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(3); dut.reset.poke(false.B)
      println("FDI_REFERENCE_BEGIN")
      def edge(): Unit = {
        dut.observation.packets.zipWithIndex.foreach { case (packet, lane) => trace(packet, lane) }
        if (dut.observation.retired.valid.peek().litToBoolean) {
          val fields = dut.observation.retired.elements.toSeq.map { case (name, value) =>
            s"\"$name\":\"${value.asInstanceOf[UInt].peek().litValue}\""
          }
          println(s"FDI_RTL_RETIRE {${fields.mkString(",")}," +
            s"\"pc\":\"${dut.observation.retiredPc.peek().litValue}\"," +
            s"\"instruction\":\"${dut.observation.retiredInstruction.peek().litValue}\"}")
        }
        dut.clock.step()
      }
      dut.observation.packets(0).valid.expect(true.B)
      dut.observation.packets(0).kind.expect(1.U)
      expectWords(dut.observation.packets(0), Map.empty)
      edge()
      val program = projectionAddresses.map(address => (address, true)) ++
        projectionAddresses.map(address => (address, false))
      for (((address, write), index) <- program.zipWithIndex) {
        val pc = BigInt(0x80000000L) + 4 * index
        val inst = instruction(address, write)
        val flag = index % 2 != 0
        dut.observation.requestFlag.poke(flag.B)
        dut.observation.flushFlag.poke(flag.B)
        dut.observation.allocate.valid.poke(true.B)
        dut.observation.allocate.bits.ptr.value.poke(4.U)
        dut.observation.allocate.bits.ptr.flag.poke(flag.B)
        dut.observation.allocate.bits.pc.poke(pc.U)
        dut.observation.allocate.bits.instr.poke(inst.U)
        edge()
        dut.observation.allocate.valid.poke(false.B)
        dut.io.request.ready.expect(true.B)
        dut.io.request.bits.instruction.poke(inst.U)
        dut.io.request.bits.operation.poke((if (write) 9 else 10).U)
        // The native reference receives x1 once at startup and executes its own image.
        dut.io.request.bits.operand.poke((if (write) allBits else BigInt(0)).U)
        dut.io.request.bits.rob.poke(4.U)
        dut.io.request.bits.basePc.poke((pc & (allBits ^ 31)).U)
        dut.io.request.bits.offset.poke(((pc & 31) >> 1).U)
        dut.io.request.valid.poke(true.B)
        dut.observation.packets(0).uid.expect((index + 1).U)
        dut.observation.packets(0).pc.expect(pc.U)
        edge()
        dut.io.request.valid.poke(false.B)
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.illegal.expect(false.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        edge()
        dut.observation.packets(1).valid.expect(true.B)
        dut.observation.packets(1).actualWrite.expect(write.B)
        edge()
        dut.observation.retire.valid.poke(true.B)
        dut.observation.retire.bits.ptr.value.poke(4.U)
        dut.observation.retire.bits.ptr.flag.poke(flag.B)
        dut.observation.retire.bits.count.poke(1.U)
        dut.observation.retired.uid.expect((index + 1).U)
        dut.observation.retired.beforeCount.expect(index.U)
        dut.observation.retired.afterCount.expect((index + 1).U)
        edge()
        dut.observation.retire.valid.poke(false.B)
        edge()
      }
      dut.observation.packets.foreach(_.valid.expect(false.B))
      println("FDI_REFERENCE_END")
    }
  }

  it should "retain missing writes and simultaneous post trap cancellation records" in {
    implicit val p: Parameters = parameters(true)
    test(new FDIReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("fdi-observer-lifecycle"))) { dut =>
      clear(dut.io.request); clear(dut.io.bank); clear(dut.io.flush); clear(dut.io.retired)
      clear(dut.io.boundary); clear(dut.io.trap)
      dut.io.epoch.poke(1.U); dut.io.cycle.poke(0.U); dut.io.writes.poke(0.U)
      dut.io.responseValid.poke(false.B); dut.io.responseIllegal.poke(false.B)
      dut.io.responseVirtual.poke(false.B); dut.io.coreReset.poke(true.B)
      dut.clock.step(2)
      dut.io.coreReset.poke(false.B)
      dut.io.packets(0).kind.expect(1.U)
      dut.clock.step()
      dut.io.cycle.poke(1.U)
      val request = dut.io.request
      request.valid.poke(true.B)
      request.bits.ptr.value.poke(4.U); request.bits.uid.poke(17.U); request.bits.live.poke(true.B)
      request.bits.pc.poke(BigInt("ffffffc000001008", 16).U)
      request.bits.instruction.poke(instruction(0x8b1, true).U)
      request.bits.address.poke(0x8b1.U); request.bits.read.poke(true.B)
      request.bits.write.poke(true.B); request.bits.permitted.poke(true.B)
      dut.clock.step()
      request.valid.poke(false.B); dut.io.cycle.poke(2.U)
      dut.io.responseValid.poke(true.B)
      // This is an observer-only injected omission, not a production state override.
      dut.io.writes.poke(0.U)
      dut.clock.step()
      dut.io.cycle.poke(3.U)
      dut.io.packets(1).valid.expect(true.B)
      dut.io.packets(1).uid.expect(17.U)
      dut.io.packets(1).actualWrite.expect(false.B)
      dut.io.packets(1).pc.expect(BigInt("ffffffc000001008", 16).U)
      dut.io.boundary.valid.poke(true.B)
      dut.io.boundary.bits.ptr.value.poke(4.U)
      dut.io.flush.valid.poke(true.B)
      dut.io.flush.bits.level.poke(RedirectLevel.flush)
      dut.io.flush.bits.robIdx.value.poke(4.U)
      dut.io.trap.valid.poke(true.B)
      dut.io.trap.bits.epoch.poke(1.U); dut.io.trap.bits.uid.poke(17.U)
      dut.io.trap.bits.eventSeq.poke(1.U); dut.io.trap.bits.cycle.poke(3.U)
      dut.io.packets(2).valid.expect(true.B)
      dut.io.packets(3).valid.expect(true.B)
      dut.io.packets(3).cancelReason.expect(2.U)
      dut.clock.step()
      dut.io.trap.valid.poke(false.B); dut.io.boundary.valid.poke(false.B)
      dut.io.flush.valid.poke(false.B); dut.io.responseValid.poke(false.B)
      dut.io.packets.foreach(_.valid.expect(false.B))
    }
  }

  it should "retain the real synchronous or asynchronous boundary until a later redirect" in {
    implicit val p: Parameters = parameters(true)
    test(new FDIReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("fdi-observer-boundary"))) { dut =>
      clear(dut.io.request); clear(dut.io.bank); clear(dut.io.flush); clear(dut.io.retired)
      clear(dut.io.boundary); clear(dut.io.trap)
      dut.io.epoch.poke(1.U); dut.io.writes.poke(0.U)
      dut.io.responseValid.poke(false.B); dut.io.responseIllegal.poke(false.B)
      dut.io.responseVirtual.poke(false.B); dut.io.coreReset.poke(true.B)
      var cycle = 0
      def edge(): Unit = { dut.clock.step(); cycle += 1; dut.io.cycle.poke(cycle.U) }
      dut.io.cycle.poke(0.U); edge(); edge()
      dut.io.coreReset.poke(false.B); edge()
      for ((asynchronous, index) <- Seq(false, true).zipWithIndex) {
        val uid = index + 1
        val request = dut.io.request
        request.valid.poke(true.B)
        request.bits.ptr.value.poke(4.U); request.bits.ptr.flag.poke(asynchronous.B)
        request.bits.uid.poke(uid.U); request.bits.live.poke(true.B)
        request.bits.pc.poke((0x80000000L + 4 * index).U)
        request.bits.instruction.poke(instruction(0x8b1, false).U)
        request.bits.address.poke(0x8b1.U); request.bits.read.poke(true.B)
        request.bits.write.poke(false.B); request.bits.permitted.poke(asynchronous.B)
        edge()
        request.valid.poke(false.B); dut.io.responseValid.poke(true.B)
        dut.io.responseIllegal.poke((!asynchronous).B)
        edge()
        dut.io.packets(1).valid.expect(true.B)
        dut.io.packets(1).responseIllegal.expect((!asynchronous).B)
        dut.io.responseValid.poke(false.B)
        dut.io.boundary.valid.poke(true.B)
        dut.io.boundary.bits.ptr.value.poke(4.U)
        dut.io.boundary.bits.ptr.flag.poke(asynchronous.B)
        dut.io.boundary.bits.isInterrupt.poke(asynchronous.B)
        edge()
        dut.io.boundary.valid.poke(false.B)
        // The boundary pulse is gone before both the architectural trap and redirect.
        dut.io.trap.valid.poke(true.B)
        dut.io.trap.bits.epoch.poke(1.U); dut.io.trap.bits.uid.poke(uid.U)
        dut.io.trap.bits.robIdx.poke(4.U); dut.io.trap.bits.robFlag.poke(asynchronous.B)
        dut.io.trap.bits.eventSeq.poke(uid.U); dut.io.trap.bits.cycle.poke(cycle.U)
        edge()
        dut.io.trap.valid.poke(false.B)
        edge()
        dut.io.flush.valid.poke(true.B)
        dut.io.flush.bits.level.poke(RedirectLevel.flush)
        dut.io.flush.bits.robIdx.value.poke(4.U)
        dut.io.flush.bits.robIdx.flag.poke(asynchronous.B)
        dut.io.packets(3).valid.expect(true.B)
        dut.io.packets(3).uid.expect(uid.U)
        dut.io.packets(3).robFlag.expect(asynchronous.B)
        dut.io.packets(3).cancelReason.expect((if (asynchronous) 3 else 2).U)
        dut.io.packets(3).responseIllegal.expect((!asynchronous).B)
        expectWords(dut.io.packets(3), Map.empty)
        edge()
        dut.io.flush.valid.poke(false.B)
        dut.io.packets.foreach(_.valid.expect(false.B))
      }
    }
  }

  it should "sample canonical trap addresses before the destination privilege becomes visible" in {
    implicit val p: Parameters = parameters(true)
    test(new FDITrapPCObservationHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("fdi-trap-address"))) { dut =>
      dut.io.allocate.poke(false.B); dut.io.boundary.poke(false.B); dut.io.trap.poke(false.B)
      dut.io.slot.poke(0.U); dut.io.pc.poke(0.U); dut.io.privilege.poke(3.U)
      dut.io.virtualMode.poke(false.B); dut.io.satpMode.poke(0.U); dut.io.vsatpMode.poke(0.U)
      dut.clock.step(2)
      val cases = Seq(
        (BigInt("4000001008", 16), 1, false, 8, 9, BigInt("ffffffc000001008", 16)),
        (BigInt("4000001008", 16), 0, false, 8, 0, BigInt("ffffffc000001008", 16)),
        (BigInt("4000001008", 16), 1, true, 0, 8, BigInt("ffffffc000001008", 16)),
        (BigInt("800000001008", 16), 0, true, 8, 9, BigInt("ffff800000001008", 16)),
        (BigInt("800000001008", 16), 1, false, 9, 0, BigInt("ffff800000001008", 16)),
        (BigInt("800000001008", 16), 0, false, 0, 9, BigInt("800000001008", 16)),
        (BigInt("1234800000001008", 16), 3, false, 9, 9, BigInt("800000001008", 16)),
        (BigInt("1234", 16), 1, true, 9, 8, BigInt("1234", 16)))
      for (((pc, mode, virtualMode, satp, vsatp, expected), index) <- cases.zipWithIndex) {
        dut.io.slot.poke(index.U); dut.io.pc.poke(pc.U); dut.io.privilege.poke(mode.U)
        dut.io.virtualMode.poke(virtualMode.B); dut.io.satpMode.poke(satp.U); dut.io.vsatpMode.poke(vsatp.U)
        dut.io.current.expect(expected.U)
        dut.io.allocate.poke(true.B); dut.clock.step(); dut.io.allocate.poke(false.B)
        dut.io.boundary.poke(true.B); dut.clock.step(); dut.io.boundary.poke(false.B)
        dut.io.trap.poke(true.B); dut.clock.step(); dut.io.trap.poke(false.B)
        dut.io.privilege.poke(3.U); dut.io.virtualMode.poke(false.B)
        dut.io.satpMode.poke(0.U); dut.io.vsatpMode.poke(0.U)
        dut.io.current.expect((pc & ((BigInt(1) << 48) - 1)).U)
        dut.io.snapshotValid.expect(true.B)
        dut.io.snapshotPC.expect(expected.U)
        dut.clock.step()
        dut.io.snapshotValid.expect(false.B)
      }
    }
  }

  it should "emit no active observation when the production feature is disabled" in {
    implicit val p: Parameters = parameters(false)
    test(new FDIReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("fdi-observer-disabled"))) { dut =>
      clear(dut.io.request); clear(dut.io.bank); clear(dut.io.flush); clear(dut.io.retired)
      clear(dut.io.boundary); clear(dut.io.trap)
      dut.io.coreReset.poke(false.B); dut.io.epoch.poke(1.U); dut.io.cycle.poke(1.U)
      dut.io.responseValid.poke(false.B); dut.io.responseIllegal.poke(false.B)
      dut.io.responseVirtual.poke(false.B); dut.io.writes.poke(0.U)
      dut.clock.step(3)
      dut.io.request.valid.poke(true.B)
      dut.io.request.bits.uid.poke(1.U); dut.io.request.bits.live.poke(true.B)
      dut.io.request.bits.address.poke(0x8b1.U)
      dut.clock.step(3)
      dut.io.packets.foreach(_.valid.expect(false.B))
    }
  }
}
