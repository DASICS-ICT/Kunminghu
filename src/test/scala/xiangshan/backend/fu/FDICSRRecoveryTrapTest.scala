// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.fu.NewCSR.FDIFReasonModule

// The input is an accepted CSR trap transaction. ROB age selection is outside
// this fixture; trap entry, native CSR owners and recovery targets are real.
class FDICSRRecoveryTrapHarness(implicit p: Parameters) extends FDICSRRecoveryHarness {
  val trapBoundary = IO(new Bundle {
    val request = Input(Valid(new Bundle {
      val vector = UInt(26.W)
      val pc = UInt(64.W)
      val instruction = UInt(32.W)
      val tval = UInt(64.W)
      val reason = UInt(3.W)
    }))
    val softwareReasonWrite = Output(Bool())
    val hardwareReasonWrite = Output(Bool())
    val hardwareReasonValue = Output(UInt(3.W))
    val fdiDistribution = Output(Bool())
    val machineEntry = Output(Bool())
    val supervisorEntry = Output(Bool())
    val targetUpdate = Output(Bool())
    val target = Output(UInt(64.W))
    val nativeState = Output(Vec(6, UInt(64.W)))
  })
  val exception = csr.io.csrio.get.exception
  exception.valid := trapBoundary.request.valid
  exception.bits.pc := trapBoundary.request.bits.pc
  exception.bits.instr := trapBoundary.request.bits.instruction
  exception.bits.exceptionVec := trapBoundary.request.bits.vector.asTypeOf(ExceptionVec())
  exception.bits.isInterrupt := false.B
  exception.bits.singleStep := false.B
  exception.bits.fdiException.foreach { record =>
    record.tval := trapBoundary.request.bits.tval
    record.reason := trapBoundary.request.bits.reason
  }

  val bank = csr.csrMod
  val reasonOwner = bank.csrMods.find(_.addr == 0x8b3).get.asInstanceOf[FDIFReasonModule]
  trapBoundary.softwareReasonWrite := observe(bank.csrRwMap(0x8b3)._1.wen)
  trapBoundary.hardwareReasonWrite := observe(reasonOwner.trapReason.valid)
  trapBoundary.hardwareReasonValue := observe(reasonOwner.trapReason.bits.REASON).asUInt
  trapBoundary.fdiDistribution := observe(bank.io.distributedFDI.get.w.valid)
  trapBoundary.machineEntry := observe(bank.trapEntryMEvent.valid)
  trapBoundary.supervisorEntry := observe(bank.trapEntryHSEvent.valid)
  trapBoundary.targetUpdate := observe(bank.io.out.bits.targetPcUpdate)
  trapBoundary.target := csr.io.csrio.get.trapTarget.pc
  Seq(0x141, 0x142, 0x143, 0x341, 0x342, 0x343).zipWithIndex.foreach { case (address, index) =>
    trapBoundary.nativeState(index) := observe(bank.csrOutMap(address))
  }
}

class FDICSRRecoveryTrapTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI hardware trap and software recovery separation"

  it should "update hardware FReason and the native trap target without a software flush obligation" in {
    val root = java.nio.file.Paths.get(sys.props("c07.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    val base = new top.DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = true)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
    test(new FDICSRRecoveryTrapHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("c07-hardware-trap"))) { dut =>
      val machineVector = BigInt("80000100", 16)
      val supervisorVector = BigInt("80000200", 16)
      // FReason is the last special-register word in the literal C05 view order.
      val reasonIndex = 42
      var cycles = 0
      var hardwareEffects = 0
      var targetUpdates = 0
      var trapWindow = false
      var requestPC = BigInt(0x1000)
      var expectedSoftwareEffect: Option[(Int, BigInt)] = None
      var softwareEffects = 0
      var distributions = 0
      var softwareFlushes = 0
      val setupViews = Map(0xbc4 -> 0, 0xbc5 -> 1, 0xbc6 -> 2, 0x9e2 -> 4, 0x9e3 -> 5)

      def checkSoftwareRecovery(): Unit = {
        dut.io.writes.expect(expectedSoftwareEffect
          .map { case (address, _) => BigInt(1) << setupViews(address) }.getOrElse(BigInt(0)).U)
        dut.trapBoundary.softwareReasonWrite.expect(false.B)
        dut.trapBoundary.fdiDistribution.expect(expectedSoftwareEffect.nonEmpty.B)
        expectedSoftwareEffect.foreach { case (address, value) =>
          dut.observed.bus.w.valid.expect(true.B)
          dut.observed.bus.w.bits.addr.expect(address.U)
          dut.observed.bus.w.bits.data.expect(value.U)
        }
        // Ordinary setup CSR writes legitimately use the shared distribution
        // bus. The trap and its subsequent reads have no such software writes.
        if (trapWindow) {
          assert(expectedSoftwareEffect.isEmpty)
          dut.observed.bus.w.valid.expect(false.B)
        }
        // Sample the exposed payload even while idle to detect a pending fact
        // before a fresh C0 could clear it. This is not a claimed response fire.
        dut.recovery.responseFlushPipe.expect(expectedSoftwareEffect.nonEmpty.B)
      }
      def edge(): Unit = {
        checkSoftwareRecovery()
        if (dut.trapBoundary.hardwareReasonWrite.peek().litToBoolean) hardwareEffects += 1
        if (dut.trapBoundary.targetUpdate.peek().litToBoolean) targetUpdates += 1
        if (dut.io.writes.peek().litValue != 0) softwareEffects += 1
        if (dut.trapBoundary.fdiDistribution.peek().litToBoolean) distributions += 1
        if (dut.recovery.responseFire.peek().litToBoolean &&
          dut.recovery.responseFlushPipe.peek().litToBoolean) softwareFlushes += 1
        dut.clock.step()
        cycles += 1
      }
      def drive(instruction: BigInt, operation: UInt, operand: BigInt): Unit = {
        dut.io.request.bits.instruction.poke(instruction.U)
        dut.io.request.bits.operation.poke(operation)
        dut.io.request.bits.operand.poke(operand.U)
        dut.io.request.bits.basePc.poke(requestPC.U)
        dut.io.request.bits.offset.poke(0.U)
        dut.io.request.bits.rob.poke(4.U)
      }
      def transact(instruction: BigInt, operation: UInt, operand: BigInt,
        stalls: Int = 0, setupEffect: Option[(Int, BigInt)] = None): BigInt = {
        require(setupEffect.isEmpty || stalls == 0)
        val effectsBefore = hardwareEffects
        dut.io.request.ready.expect(true.B)
        drive(instruction, operation, operand)
        dut.io.request.valid.poke(true.B)
        dut.io.pc.expect(requestPC.U)
        dut.io.response.ready.poke((stalls == 0).B)
        edge()
        expectedSoftwareEffect = setupEffect
        dut.io.request.valid.poke(false.B)
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.illegal.expect(false.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        val value = dut.io.response.bits.data.peek().litValue
        for (_ <- 0 until stalls) {
          dut.io.response.valid.expect(true.B)
          dut.io.response.bits.data.expect(value.U)
          dut.recovery.responseFire.expect(false.B)
          edge()
        }
        dut.io.response.ready.poke(true.B)
        dut.recovery.responseFire.expect(true.B)
        edge()
        expectedSoftwareEffect = None
        dut.io.response.valid.expect(false.B)
        dut.io.request.ready.expect(true.B)
        checkSoftwareRecovery()
        assert(hardwareEffects == effectsBefore, "A software access repeated the hardware reason effect")
        value
      }
      def write(address: Int, value: BigInt): Unit = {
        val instruction = (BigInt(address) << 20) | (1 << 15) | (1 << 12) | (1 << 7) | 0x73
        transact(instruction, 9.U, value,
          setupEffect = if (setupViews.contains(address)) Some((address, value)) else None)
        setupViews.get(address).foreach(index => dut.io.state(index).expect(value.U))
      }
      def read(address: Int, stalls: Int = 0): BigInt =
        transact((BigInt(address) << 20) | (2 << 12) | (1 << 7) | 0x73, 10.U, 0, stalls)
      def reset(): Unit = {
        trapWindow = false
        requestPC = 0x1000
        expectedSoftwareEffect = None
        dut.io.request.valid.poke(false.B)
        drive(BigInt("340020f3", 16), 10.U, 0)
        dut.io.response.ready.poke(true.B)
        dut.io.flush.poke(false.B)
        dut.io.flushRob.poke(4.U)
        dut.io.trap.poke(false.B)
        dut.recovery.requestFlag.poke(false.B)
        dut.recovery.flushFlag.poke(false.B)
        dut.recovery.flushAfter.poke(false.B)
        dut.trapBoundary.request.valid.poke(false.B)
        dut.trapBoundary.request.bits.vector.poke(0.U)
        dut.trapBoundary.request.bits.pc.poke(0.U)
        dut.trapBoundary.request.bits.instruction.poke(0.U)
        dut.trapBoundary.request.bits.tval.poke(0.U)
        dut.trapBoundary.request.bits.reason.poke(0.U)
        dut.reset.poke(true.B)
        dut.clock.step(5)
        dut.reset.poke(false.B)
        hardwareEffects = 0
        targetUpdates = 0
        softwareEffects = 0
        distributions = 0
        softwareFlushes = 0
        edge()
        dut.io.mode.expect(3.U)
        dut.io.virtualMode.expect(false.B)
        dut.io.state(reasonIndex).expect(0.U)
      }

      // Both source modes are entered through native CSR writes and MRET.
      // M-mode is never presented as the origin of a DASICS protection fault.
      for ((sourceMode, cause, delegated, reason) <- Seq((0, 24, false, 2), (1, 25, true, 4))) {
        reset()
        val faultPC = BigInt("80001000", 16) + sourceMode * 0x100
        val faultValue = BigInt("80004000", 16) + sourceMode * 0x1000
        val faultInstruction = if (sourceMode == 0) BigInt("00033283", 16) else BigInt("00030067", 16)
        val expectedTarget = if (delegated) supervisorVector else machineVector
        val boundLo = if (sourceMode == 0) 0x9e2 else 0xbc5
        val boundHi = if (sourceMode == 0) 0x9e3 else 0xbc6
        val enable = if (sourceMode == 0) 2 else 1
        write(0x305, machineVector)
        write(0x105, supervisorVector)
        write(0x30c, (BigInt(1) << 63) | 1)
        write(0x60c, (BigInt(1) << 63) | 1)
        write(0x10c, 1)
        write(0x302, if (delegated) BigInt(1) << cause else BigInt(0))
        // Only the handler is trusted. Source PC and aligned access/target are
        // outside that interval; zero LibCfg/JumpCfg supply no denial bypass.
        write(boundLo, expectedTarget)
        write(boundHi, expectedTarget + 0x100)
        write(0xbc4, enable)
        write(0x300, BigInt(sourceMode) << 11)
        write(0x341, faultPC)
        transact(BigInt("30200073", 16), CSROpType.jmp, 0)
        dut.trapBoundary.target.expect(faultPC.U)
        requestPC = faultPC
        dut.io.mode.expect(sourceMode.U)
        dut.io.virtualMode.expect(false.B)
        for (_ <- 0 until 2) edge()
        dut.io.state(0).expect(enable.U)
        dut.io.state(6).expect(0.U) // LibCfg has no valid permission entries.
        dut.io.state(51).expect(0.U) // JumpCfg has no valid target entries.
        Seq(39, 40, 41).foreach(index => dut.io.state(index).expect(0.U))
        assert(faultPC < expectedTarget || faultPC >= expectedTarget + 0x100)
        assert(faultValue < expectedTarget || faultValue >= expectedTarget + 0x100)
        assert((faultPC & 3) == 0 && (faultValue & 7) == 0)
        assert(softwareEffects == 3 && distributions == 3 && softwareFlushes == 3)
        println(s"C07_TRAP_SETUP_PASS source=$sourceMode fdiEffects=3 fdiDistributions=3 " +
          s"softwareFlushes=3 mainCfg=$enable handlerLo=0x${expectedTarget.toString(16)} " +
          s"handlerHi=0x${(expectedTarget + 0x100).toString(16)} sourcePc=0x${faultPC.toString(16)}")
        trapWindow = true

        val effectsBefore = hardwareEffects
        val updatesBefore = targetUpdates
        val nativeIndex = if (delegated) 0 else 3
        val untouchedIndex = if (delegated) 3 else 0
        val untouched = (0 until 3).map(i => dut.trapBoundary.nativeState(untouchedIndex + i).peek().litValue)
        dut.io.request.valid.expect(false.B)
        dut.io.response.valid.expect(false.B)
        dut.trapBoundary.request.bits.vector.poke((BigInt(1) << cause).U)
        dut.trapBoundary.request.bits.pc.poke(faultPC.U)
        // LD x5, 0(x6) uses the recorded address; JALR x0, 0(x6) uses
        // the recorded target. The accepted record comes from upstream logic.
        dut.trapBoundary.request.bits.instruction.poke(faultInstruction.U)
        dut.trapBoundary.request.bits.tval.poke(faultValue.U)
        dut.trapBoundary.request.bits.reason.poke(reason.U)
        dut.trapBoundary.request.valid.poke(true.B)
        dut.trapBoundary.hardwareReasonWrite.expect(true.B)
        dut.trapBoundary.hardwareReasonValue.expect(reason.U)
        dut.trapBoundary.machineEntry.expect((!delegated).B)
        dut.trapBoundary.supervisorEntry.expect(delegated.B)
        dut.trapBoundary.targetUpdate.expect(true.B)
        dut.trapBoundary.target.expect(expectedTarget.U)
        edge()
        dut.trapBoundary.request.valid.poke(false.B)
        requestPC = expectedTarget
        dut.io.mode.expect((if (delegated) 1 else 3).U)
        dut.io.state(reasonIndex).expect(reason.U)
        dut.trapBoundary.nativeState(nativeIndex).expect(faultPC.U)
        dut.trapBoundary.nativeState(nativeIndex + 1).expect(cause.U)
        dut.trapBoundary.nativeState(nativeIndex + 2).expect(faultValue.U)
        untouched.zipWithIndex.foreach { case (value, i) =>
          dut.trapBoundary.nativeState(untouchedIndex + i).expect(value.U)
        }
        // A normal trap still changes the recovery target; it creates no
        // accepted software response and must not arm C07's response retention.
        for (_ <- 0 until 4) {
          dut.io.response.valid.expect(false.B)
          dut.trapBoundary.hardwareReasonWrite.expect(false.B)
          dut.trapBoundary.targetUpdate.expect(false.B)
          dut.trapBoundary.target.expect(expectedTarget.U)
          edge()
        }
        assert(hardwareEffects == effectsBefore + 1)
        assert(targetUpdates == updatesBefore + 1)
        assert(read(0x8b3, stalls = 3) == reason)
        assert(read(if (delegated) 0x143 else 0x343) == faultValue)
        assert(hardwareEffects == effectsBefore + 1 && targetUpdates == updatesBefore + 1)
        assert(softwareEffects == 3 && distributions == 3 && softwareFlushes == 3)
        println(s"C07_HARDWARE_TRAP_PASS source=$sourceMode cause=$cause reason=$reason " +
          s"target=0x${expectedTarget.toString(16)} hardwareEffects=1 targetUpdates=1 " +
          "scope=trap-and-post-trap-reads softwareEffects=0 fdiDistributions=0 " +
          "softwareFlush=0 freshReadStalls=3 wrapper=production")
      }
      println(s"C07_HARDWARE_TRAP_BOUNDARY_PASS cycles=$cycles cases=2 robAgeSelection=outside-fixture")
    }
  }
}
