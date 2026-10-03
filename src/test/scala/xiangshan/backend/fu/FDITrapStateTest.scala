// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.decode.Imm_Z
import xiangshan.backend.fu.NewCSR.{CSRBundle, FDIFReasonModule}
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.fu.NewCSR.CSREvents.{InterruptEventIdentity, TrapEntryHSEventModule, TrapEntryMEventModule}
import xiangshan.backend.fu.wrapper.CSR

object FDITrapTestAddresses {
  // Literal architectural views keep the oracle independent of CSR registration.
  val all = Seq(0x141, 0x142, 0x143, 0x341, 0x342, 0x343, 0x34b, 0x8b3,
    0x302, 0x602, 0x300, 0x041, 0x042, 0x043, 0x741, 0x742, 0x744)
}

// Observe the production owners without a test override of privilege or state.
class FDITrapStateProbe(implicit p: Parameters) extends CSR(FuConfig.CsrCfg) {
  val observed = IO(Output(new Bundle {
    val state = Vec(FDITrapTestAddresses.all.size, UInt(64.W))
    val mode = UInt(2.W)
    val virtualMode = Bool()
    val debugMode = Bool()
    val softwareReasonWrite = Bool()
    val hardwareReasonWrite = Bool()
    val hardwareReasonValue = UInt(3.W)
  }))
  FDITrapTestAddresses.all.zipWithIndex.foreach { case (address, index) =>
    observed.state(index) := csrMod.csrOutMap.get(address).map(BoringUtils.bore(_)).getOrElse(0.U)
  }
  observed.mode := csrMod.io.status.privState.PRVM.asUInt
  observed.virtualMode := csrMod.io.status.privState.V.asUInt.asBool
  observed.debugMode := csrMod.io.status.debugMode
  observed.softwareReasonWrite := csrMod.csrRwMap.get(0x8b3)
    .map(entry => BoringUtils.bore(entry._1.wen)).getOrElse(false.B)
  private val reasonOwner = csrMod.csrMods.find(_.addr == 0x8b3).map(_.asInstanceOf[FDIFReasonModule])
  observed.hardwareReasonWrite := reasonOwner.map(owner => BoringUtils.bore(owner.trapReason.valid))
    .getOrElse(false.B)
  observed.hardwareReasonValue := reasonOwner.map(owner => BoringUtils.bore(owner.trapReason.bits.REASON).asUInt)
    .getOrElse(0.U(3.W))
}

class FDITrapStateHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new Bundle {
      val instruction = UInt(32.W)
      val operation = UInt(6.W)
      val operand = UInt(64.W)
    }))
    val response = Decoupled(new Bundle {
      val data = UInt(64.W)
      val illegal = Bool()
      val virtualIllegal = Bool()
    })
    val trap = Input(Valid(new Bundle {
      val vector = UInt(26.W)
      val pc = UInt(64.W)
      val tval = UInt(64.W)
      val reason = UInt(3.W)
      val memVA = UInt(64.W)
      val interrupt = Bool()
      val interruptCause = UInt(8.W)
      val singleStep = Bool()
    }))
    val flush = Input(Bool())
    val nmiSource = Input(Bool())
    val acceptNmi = Input(Bool())
    val useSavedNmi = Input(Bool())
    val nmiCandidate = Output(Bool())
    val nmiCause = Output(UInt(8.W))
    val state = Output(Vec(FDITrapTestAddresses.all.size, UInt(64.W)))
    val mode = Output(UInt(2.W))
    val virtualMode = Output(Bool())
    val debugMode = Output(Bool())
    val softwareReasonWrite = Output(Bool())
    val hardwareReasonWrite = Output(Bool())
    val hardwareReasonValue = Output(UInt(3.W))
    val target = Output(UInt(64.W))
  })
  val csr = Module(new FDITrapStateProbe)
  def tieInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(tieInputs)
    case vector: Vec[_] => vector.foreach(tieInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  tieInputs(csr.io)
  csr.huEntry.foreach { port => tieInputs(port); port.completion.ready := true.B }
  csr.io.in.valid := io.request.valid
  csr.io.in.bits.ctrl.fuOpType := io.request.bits.operation
  csr.io.in.bits.ctrl.robIdx.value := 4.U
  csr.io.in.bits.data.pc.get := 0x1000.U
  csr.io.in.bits.data.imm := Imm_Z().minBitsFromInstr(io.request.bits.instruction)
  csr.io.in.bits.data.src(0) := io.request.bits.operand
  io.request.ready := csr.io.in.ready
  csr.io.out.ready := io.response.ready
  io.response.valid := csr.io.out.valid
  io.response.bits.data := csr.io.out.bits.res.data
  io.response.bits.illegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
  io.response.bits.virtualIllegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.virtualInstr)
  csr.io.flush.valid := io.flush
  csr.io.flush.bits.level := RedirectLevel.flush
  csr.io.flush.bits.robIdx.value := 4.U
  val exception = csr.io.csrio.get.exception
  exception.valid := io.trap.valid
  exception.bits.pc := io.trap.bits.pc
  exception.bits.exceptionVec := io.trap.bits.vector.asTypeOf(ExceptionVec())
  exception.bits.instr := 0x00000013.U
  exception.bits.isInterrupt := io.trap.bits.interrupt
  exception.bits.singleStep := io.trap.bits.singleStep
  exception.bits.fdiException.foreach { record =>
    record.tval := io.trap.bits.tval
    record.reason := io.trap.bits.reason
  }
  exception.bits.interruptEvent.foreach { event =>
    event.interrupt.cause := io.trap.bits.interruptCause
    event.interrupt.isInterrupt := io.trap.bits.interrupt
  }
  csr.io.csrio.get.externalInterrupt.nmi.nmi_31 := io.nmiSource
  io.nmiCandidate := false.B
  io.nmiCause := 0.U
  csr.io.csrio.get.userTimerDelivery.foreach { delivery =>
    // Claim the descriptor selected by the real interrupt module before its
    // later trap delivery. No test-defined NMI cause bypasses that selection.
    val identity = WireInit(0.U.asTypeOf(new InterruptEventIdentity))
    identity.interrupt := delivery.candidate.bits
    identity.robIdx.value := 4.U
    val canClaim = delivery.candidate.valid && delivery.candidate.bits.nmi
    val claim = io.acceptNmi && canClaim
    val saved = RegEnable(identity, claim)
    delivery.accepted.valid := claim
    delivery.accepted.bits := identity
    io.nmiCandidate := canClaim
    io.nmiCause := delivery.candidate.bits.cause
    when(io.acceptNmi) { assert(canClaim) }
    when(io.useSavedNmi) { exception.bits.interruptEvent.get := saved }
  }
  csr.io.csrio.get.memExceptionVAddr := io.trap.bits.memVA
  io.state := csr.observed.state
  io.mode := csr.observed.mode
  io.virtualMode := csr.observed.virtualMode
  io.debugMode := csr.observed.debugMode
  io.softwareReasonWrite := csr.observed.softwareReasonWrite
  io.hardwareReasonWrite := csr.observed.hardwareReasonWrite
  io.hardwareReasonValue := csr.observed.hardwareReasonValue
  io.target := csr.io.csrio.get.trapTarget.pc
}

// Auxiliary classification flags are exercised directly on the real event
// modules; they are not forged as inconsistent ROB/TrapTval transactions.
class FDITrapEventHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val cause = Input(UInt(6.W))
    val pc = Input(UInt(50.W))
    val tval = Input(UInt(64.W))
    val fetchMalTval = Input(UInt(64.W))
    val auxiliaryFlags = Input(Bool())
    val translation = Input(UInt(4.W))
    val doubleTrap = Input(Bool())
    val interrupt = Input(Bool())
    val epc = Output(Vec(2, UInt(64.W)))
    val value = Output(Vec(2, UInt(64.W)))
    val causeOut = Output(Vec(2, UInt(64.W)))
    val validOut = Output(Vec(2, Bool()))
    val mtval2 = Output(UInt(64.W))
  })
  val machine = Module(new TrapEntryMEventModule)
  val supervisor = Module(new TrapEntryHSEventModule)
  machine.valid := io.valid
  supervisor.valid := io.valid
  for (input <- Seq(machine.in, supervisor.in)) {
    input.elements.values.foreach {
      case bundle: CSRBundle => bundle := bundle.cloneType.init
      case input => input := 0.U.asTypeOf(input)
    }
    input.privState := PrivState.ModeHU
    input.iMode := PrivState.ModeHU
    input.dMode := PrivState.ModeHU
    input.satp.MODE := io.translation
    input.causeNO.ExceptionCode := io.cause
    input.causeNO.Interrupt := io.interrupt
    input.trapPc := io.pc
    input.isFetchMalAddr := io.auxiliaryFlags
    input.isCrossPageIPF := io.auxiliaryFlags
    input.isHls := io.auxiliaryFlags
    input.fetchMalTval := io.fetchMalTval
    input.memExceptionVAddr := 0x12345678.U
    input.hasDTExcp := io.doubleTrap
    input.fdiException.foreach { record =>
      record.tval := io.tval
      record.reason := 2.U
    }
  }
  io.epc(0) := machine.out.mepc.bits.asUInt
  io.epc(1) := supervisor.out.sepc.bits.asUInt
  io.value(0) := machine.out.mtval.bits.ALL.asUInt
  io.value(1) := supervisor.out.stval.bits.ALL.asUInt
  io.causeOut(0) := machine.out.mcause.bits.asUInt
  io.causeOut(1) := supervisor.out.scause.bits.asUInt
  io.validOut(0) := machine.out.mcause.valid
  io.validOut(1) := supervisor.out.scause.valid
  io.mtval2 := machine.out.mtval2.bits.ALL.asUInt
}

class FDITrapStateTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI production trap state"
  private val allOnes = (BigInt(1) << 64) - 1
  private val delegation = (BigInt(1) << 24) | (BigInt(1) << 25)
  private val machineVector = BigInt("80000100", 16)
  private val supervisorVector = BigInt("80000200", 16)

  private def parameters(enabled: Boolean): Parameters = {
    val base = new top.DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
  }
  private def run(name: String, enabled: Boolean = true)(body: Driver => Unit): Unit = {
    val root = java.nio.file.Paths.get(sys.props("e02.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    implicit val p: Parameters = parameters(enabled)
    test(new FDITrapStateHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation(name))) { dut =>
      val driver = new Driver(dut)
      driver.reset()
      body(driver)
      println(s"FDI trap state PASS group=$name cycles=${driver.cycles} " +
        s"transactions=${driver.transactions} traps=${driver.traps} " +
        s"hardwareReasonPulsesSinceReset=${driver.hardwareReasonEffects} wrapper=production")
    }
  }

  private class Driver(val dut: FDITrapStateHarness) {
    var cycles = 0
    var transactions = 0
    var traps = 0
    private var hardwareReasons = Vector.empty[BigInt]
    private var resetting = true
    def hardwareReasonEffects: Int = hardwareReasons.size
    def edge(): Unit = {
      if (!resetting && dut.io.hardwareReasonWrite.peek().litToBoolean) {
        dut.io.softwareReasonWrite.expect(false.B)
        hardwareReasons :+= dut.io.hardwareReasonValue.peek().litValue
      }
      dut.clock.step()
      cycles += 1
    }
    def idle(count: Int): Unit = {
      val before = hardwareReasonEffects
      for (_ <- 0 until count) edge()
      assert(hardwareReasonEffects == before, "An idle edge repeated a hardware reason effect")
    }
    def raw(address: Int): BigInt = dut.io.state(FDITrapTestAddresses.all.indexOf(address)).peek().litValue
    def snapshot(addresses: Seq[Int]): Map[Int, BigInt] = addresses.map(a => a -> raw(a)).toMap
    def unchanged(saved: Map[Int, BigInt]): Unit = saved.foreach { case (a, value) =>
      assert(raw(a) == value, f"unexpected state update at 0x$a%x")
    }
    def drive(address: Int, function: Int, operand: BigInt): Unit = {
      val rs1 = if (function == 2 && operand == 0) 0 else 1
      dut.io.request.bits.instruction.poke(((BigInt(address) << 20) |
        (rs1 << 15) | (function << 12) | (1 << 7) | 0x73).U)
      dut.io.request.bits.operation.poke((8 | function).U)
      dut.io.request.bits.operand.poke(operand.U)
    }
    def reset(): Unit = {
      resetting = true
      hardwareReasons = Vector.empty
      dut.io.request.valid.poke(false.B)
      drive(0x340, 2, 0)
      dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.nmiSource.poke(false.B)
      dut.io.acceptNmi.poke(false.B)
      dut.io.useSavedNmi.poke(false.B)
      dut.io.trap.valid.poke(false.B)
      dut.io.trap.bits.vector.poke(0.U)
      dut.io.trap.bits.pc.poke(0.U)
      dut.io.trap.bits.tval.poke(0.U)
      dut.io.trap.bits.reason.poke(0.U)
      dut.io.trap.bits.memVA.poke(0.U)
      dut.io.trap.bits.interrupt.poke(false.B)
      dut.io.trap.bits.interruptCause.poke(0.U)
      dut.io.trap.bits.singleStep.poke(false.B)
      dut.reset.poke(true.B)
      for (_ <- 0 until 5) edge()
      dut.reset.poke(false.B)
      resetting = false
      edge()
      dut.io.mode.expect(3.U)
      dut.io.virtualMode.expect(false.B)
      assert(raw(0x8b3) == 0)
      assert(hardwareReasonEffects == 0)
    }
    def access(address: Int, function: Int = 2, operand: BigInt = 0,
      stalls: Int = 0, illegal: Boolean = false): BigInt = {
      val before = hardwareReasonEffects
      dut.io.request.ready.expect(true.B)
      drive(address, function, operand)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke((stalls == 0).B)
      edge()
      transactions += 1
      dut.io.request.valid.poke(false.B)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(illegal.B)
      dut.io.response.bits.virtualIllegal.expect(false.B)
      val value = dut.io.response.bits.data.peek().litValue
      edge()
      for (_ <- 0 until stalls) {
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.data.expect(value.U)
        dut.io.softwareReasonWrite.expect(false.B)
        edge()
      }
      if (stalls > 0) {
        dut.io.response.ready.poke(true.B)
        edge()
      }
      dut.io.response.valid.expect(false.B)
      assert(hardwareReasonEffects == before, "A software transaction repeated a hardware reason effect")
      value
    }
    def write(address: Int, value: BigInt, stalls: Int = 0): Unit =
      access(address, 1, value, stalls)
    def setup(enabled: Boolean = true): Unit = {
      write(0x300, 0)
      write(0x305, machineVector)
      write(0x105, supervisorVector)
      if (enabled) {
        write(0x30c, (BigInt(1) << 63) | 1)
        write(0x60c, (BigInt(1) << 63) | 1)
        write(0x10c, 1)
      }
    }
    def system(instruction: BigInt): Unit = {
      val before = hardwareReasonEffects
      dut.io.request.ready.expect(true.B)
      dut.io.request.bits.instruction.poke(instruction.U)
      dut.io.request.bits.operation.poke(CSROpType.jmp)
      dut.io.request.valid.poke(true.B)
      edge()
      transactions += 1
      dut.io.request.valid.poke(false.B)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(false.B)
      dut.io.response.bits.virtualIllegal.expect(false.B)
      edge()
      assert(hardwareReasonEffects == before)
    }
    def mode(privilege: Int): Unit = {
      dut.io.mode.expect(3.U)
      write(0x300, BigInt(privilege) << 11)
      write(0x341, 0x1000)
      system(BigInt("30200073", 16))
      dut.io.mode.expect(privilege.U)
      dut.io.virtualMode.expect(false.B)
    }
    def claimNmi(): Unit = {
      val before = hardwareReasonEffects
      dut.io.nmiSource.poke(true.B)
      edge()
      dut.io.nmiSource.poke(false.B)
      var waited = 0
      while (!dut.io.nmiCandidate.peek().litToBoolean && waited < 8) {
        edge()
        waited += 1
      }
      dut.io.nmiCandidate.expect(true.B)
      dut.io.nmiCause.expect(31.U)
      dut.io.acceptNmi.poke(true.B)
      edge()
      dut.io.acceptNmi.poke(false.B)
      assert(hardwareReasonEffects == before)
    }
    def inject(vector: BigInt, pc: BigInt, tval: BigInt, reason: Int,
      memVA: BigInt = 0, interrupt: Boolean = false, debug: Boolean = false,
      nmi: Boolean = false, hardwareReason: Option[Int] = None): Map[Int, BigInt] = {
      val before = hardwareReasonEffects
      dut.io.request.valid.expect(false.B)
      dut.io.response.valid.expect(false.B)
      dut.io.trap.bits.vector.poke(vector.U)
      dut.io.trap.bits.pc.poke(pc.U)
      dut.io.trap.bits.tval.poke(tval.U)
      dut.io.trap.bits.reason.poke(reason.U)
      dut.io.trap.bits.memVA.poke(memVA.U)
      dut.io.trap.bits.interrupt.poke(interrupt.B)
      dut.io.trap.bits.interruptCause.poke(7.U)
      dut.io.trap.bits.singleStep.poke(debug.B)
      dut.io.useSavedNmi.poke(nmi.B)
      dut.io.trap.valid.poke(true.B)
      edge()
      traps += 1
      // Capture all observed native owners at the effect edge, before another
      // edge or CSR read can hide a delayed or partially committed record.
      val committed = snapshot(FDITrapTestAddresses.all)
      dut.io.trap.valid.poke(false.B)
      edge()
      assert(hardwareReasons.drop(before) == hardwareReason.toSeq.map(value => BigInt(value)),
        s"Unexpected hardware reason effects: ${hardwareReasons.drop(before)}")
      committed
    }
  }

  it should "commit delegated and machine faults with paired state and preserve unrelated traps" in run("trap-state-on") { d =>
    val cases = for (cause <- Seq(24, 25); delegated <- Seq(false, true); reason <- 1 to 4)
      yield (cause, delegated, reason)
    for ((cause, delegated, reason) <- cases) {
      d.reset(); d.setup()
      d.write(0x8b3, 7, stalls = reason)
      d.write(0x302, if (delegated) BigInt(1) << cause else BigInt(0))
      d.mode(if (cause == 24) 0 else 1)
      val untouched = d.snapshot(if (delegated) Seq(0x341, 0x342, 0x343, 0x34b, 0x041, 0x042, 0x043)
        else Seq(0x141, 0x142, 0x143, 0x041, 0x042, 0x043))
      val pc = BigInt("80001002", 16) + reason * 4
      val value = Seq(BigInt(0), BigInt("fedcba9876543211", 16),
        BigInt("80002003", 16), BigInt("ffffffff80003005", 16))(reason - 1)
      val committed = d.inject(BigInt(1) << cause, pc, value, reason, hardwareReason = Some(reason))
      d.dut.io.mode.expect((if (delegated) 1 else 3).U)
      d.dut.io.target.expect((if (delegated) supervisorVector else machineVector).U)
      val epc = if (delegated) 0x141 else 0x341
      assert(committed(epc) == pc)
      assert(committed(epc + 1) == cause)
      assert(committed(epc + 2) == value)
      assert(committed(0x8b3) == reason)
      untouched.foreach { case (address, previous) => assert(committed(address) == previous) }
      assert(d.access(epc) == pc)
      assert(d.access(epc + 1) == cause)
      assert(d.access(epc + 2) == value)
      assert(d.access(0x8b3) == reason)
      d.unchanged(untouched)
      d.write(0x8b3, 6, stalls = 2)
      assert(d.access(0x8b3) == 6)
    }
    for (cause <- Seq(24, 25); standard <- Seq(1, 5, if (cause == 24) 8 else 9)) {
      d.reset(); d.setup(); d.write(0x8b3, 7)
      d.mode(if (cause == 24) 0 else 1)
      val pc = BigInt("80001236", 16)
      val mem = BigInt("ffffffc001234567", 16)
      d.inject((BigInt(1) << cause) | (BigInt(1) << standard), pc, allOnes, 2, memVA = mem)
      assert(d.access(0x342) == standard)
      assert(d.access(0x341) == pc)
      assert(d.access(0x343) == (if (standard == 1) pc else if (standard == 5) mem else BigInt(0)))
      assert(d.access(0x8b3) == 7)
    }
    for (debug <- Seq(false, true)) {
      d.reset(); d.setup(); d.write(0x8b3, 6); d.mode(0)
      d.inject(BigInt(1) << 24, 0x80001236L, allOnes, 2, interrupt = !debug, debug = debug)
      assert(d.raw(0x8b3) == 6)
      if (debug) d.dut.io.debugMode.expect(true.B)
      else {
        assert(d.access(0x342) == ((BigInt(1) << 63) | 7))
        assert(d.access(0x343) == 0)
      }
    }
  }

  it should "retain exact reason effects through handlers and repeated trap returns without reset" in run("trap-return-sequences") { d =>
    for (delegated <- Seq(false, true)) {
      d.reset(); d.setup()
      d.write(0x302, if (delegated) BigInt(1) << 24 else BigInt(0))
      d.write(0x8b3, 7, stalls = 3)
      d.mode(0)
      val epc = if (delegated) 0x141 else 0x341
      val otherBank = if (delegated) Seq(0x341, 0x342, 0x343, 0x34b) else Seq(0x141, 0x142, 0x143)
      val records = Seq(
        (BigInt("80002236", 16), BigInt("ffffffff80003001", 16), 2),
        (BigInt("8000243a", 16), BigInt("fedcba9876543003", 16), 4),
        (BigInt("8000263e", 16), BigInt(0), 1))
      records.zipWithIndex.foreach { case ((pc, tval, reason), index) =>
        d.dut.io.mode.expect(0.U)
        d.idle(index + 1)
        val preserved = d.snapshot(otherBank ++ Seq(0x041, 0x042, 0x043))
        val committed = d.inject(BigInt(1) << 24, pc, tval, reason, hardwareReason = Some(reason))
        assert(d.hardwareReasonEffects == index + 1)
        assert(committed(epc) == pc)
        assert(committed(epc + 1) == 24)
        assert(committed(epc + 2) == tval)
        assert(committed(0x8b3) == reason)
        preserved.foreach { case (address, previous) => assert(committed(address) == previous) }
        d.dut.io.mode.expect((if (delegated) 1 else 3).U)
        assert(d.access(epc) == pc)
        assert(d.access(epc + 1) == 24)
        assert(d.access(epc + 2) == tval)
        assert(d.access(0x8b3, stalls = index + 2) == reason)
        val softwareValue = Seq(6, 5, 7)(index)
        d.write(0x8b3, softwareValue, stalls = 3)
        assert(d.access(0x8b3) == softwareValue)
        d.idle(3)
        assert(d.raw(0x8b3) == softwareValue)
        assert(d.hardwareReasonEffects == index + 1)
        if (index + 1 < records.size) {
          d.write(epc, records(index + 1)._1)
          d.system(BigInt(if (delegated) "10200073" else "30200073", 16))
          d.dut.io.mode.expect(0.U)
          d.dut.io.virtualMode.expect(false.B)
          assert(d.raw(0x8b3) == softwareValue)
          assert(d.hardwareReasonEffects == index + 1)
        }
      }
      assert(d.hardwareReasonEffects == 3)
    }
  }

  it should "keep WARL delegation, cancellation and double trap behavior precise" in run("trap-boundaries-on") { d =>
    d.setup()
    d.write(0x302, delegation)
    assert(d.access(0x302) == delegation)
    d.access(0x302, 3, BigInt(1) << 24)
    assert(d.access(0x302) == (BigInt(1) << 25))
    d.access(0x302, 2, BigInt(1) << 24)
    assert(d.access(0x302) == delegation)
    d.write(0x602, allOnes)
    assert((d.access(0x602) & delegation) == 0)
    d.write(0x8b3, 5)
    val before = d.snapshot(FDITrapTestAddresses.all)
    d.dut.io.trap.bits.vector.poke(delegation.U)
    d.dut.io.trap.bits.tval.poke(allOnes.U)
    d.dut.io.trap.bits.reason.poke(7.U)
    for (_ <- 0 until 4) d.edge()
    d.unchanged(before)
    d.drive(0x8b3, 1, 3)
    d.dut.io.request.valid.poke(true.B)
    d.edge()
    d.dut.io.request.valid.poke(false.B)
    d.dut.io.flush.poke(true.B)
    d.dut.io.softwareReasonWrite.expect(false.B)
    d.edge()
    d.dut.io.flush.poke(false.B)
    assert(d.raw(0x8b3) == 5)
    assert(d.hardwareReasonEffects == 0)
    d.dut.io.response.valid.expect(false.B)
    d.drive(0x8b3, 1, 4)
    d.dut.io.request.valid.poke(true.B)
    d.edge()
    d.reset()
    assert(d.raw(0x8b3) == 0)

    // S-mode double trap redirects to M with cause 16. The original fault's
    // payload survives in trap CSRs, while FReason has no DASICS trap effect.
    d.setup(); d.write(0x8b3, 6)
    d.write(0x302, BigInt(1) << 25)
    d.write(0x30a, BigInt(1) << 59)
    d.mode(1)
    d.write(0x100, BigInt(1) << 24)
    assert((d.raw(0x300) & (BigInt(1) << 24)) != 0)
    val oldSupervisor = d.snapshot(Seq(0x141, 0x142, 0x143))
    val value = BigInt("fedcba9876543211", 16)
    val committed = d.inject(BigInt(1) << 25, 0x80001436L, value, 3)
    d.dut.io.mode.expect(3.U)
    assert(committed(0x342) == 16)
    assert(committed(0x341) == 0x80001436L)
    assert(committed(0x343) == value)
    assert(committed(0x34b) == 25)
    assert(committed(0x8b3) == 6)
    oldSupervisor.foreach { case (address, previous) => assert(committed(address) == previous) }
    assert(d.access(0x342) == 16)
    assert(d.access(0x341) == 0x80001436L)
    assert(d.access(0x343) == value)
    assert(d.access(0x34b) == 25)
    assert(d.access(0x8b3) == 6)
    d.unchanged(oldSupervisor)

    d.reset(); d.setup(); d.write(0x8b3, 5)
    d.write(0x744, 0)
    assert((d.access(0x744) & 8) == 0)
    d.mode(1)
    val disabledBefore = d.snapshot(FDITrapTestAddresses.all)
    val suppressed = d.inject(BigInt(1) << 25, 0x80001836L, allOnes, 3)
    disabledBefore.foreach { case (address, previous) => assert(suppressed(address) == previous) }
    d.unchanged(disabledBefore)
    d.dut.io.mode.expect(1.U)

    // A standard M-mode exception while MDT is set enters MN. Generating a
    // DASICS fault in M would not represent an enabled producer transaction.
    d.reset(); d.setup(); d.write(0x8b3, 6)
    d.write(0x300, BigInt(1) << 42)
    val machineBefore = d.snapshot(Seq(0x141, 0x142, 0x143, 0x341, 0x342, 0x343, 0x34b, 0x8b3))
    val machineDouble = d.inject(BigInt(1) << 11, 0x80001a36L, 0, 0)
    machineBefore.foreach { case (address, previous) => assert(machineDouble(address) == previous) }
    assert(machineDouble(0x741) == 0x80001a36L)
    assert(machineDouble(0x742) == 11)
    assert((machineDouble(0x744) & 8) == 0)
    d.unchanged(machineBefore)

    d.reset(); d.setup(); d.write(0x8b3, 6); d.mode(1)
    d.claimNmi()
    val nmiBefore = d.snapshot(Seq(0x141, 0x142, 0x143, 0x341, 0x342, 0x343, 0x34b, 0x8b3))
    val nmiState = d.inject(0, 0x80001c36L, 0, 0, interrupt = true, nmi = true)
    nmiBefore.foreach { case (address, previous) => assert(nmiState(address) == previous) }
    assert(nmiState(0x741) == 0x80001c36L)
    assert(nmiState(0x742) == ((BigInt(1) << 63) | 31))
    assert((nmiState(0x744) & 8) == 0)
    d.dut.io.mode.expect(3.U)
    d.unchanged(nmiBefore)
  }

  it should "keep optional delegation read only zero when disabled" in run("trap-state-off", enabled = false) { d =>
    d.setup(enabled = false)
    d.write(0x302, delegation)
    assert((d.access(0x302) & delegation) == 0)
    d.write(0x602, allOnes)
    assert((d.access(0x602) & delegation) == 0)
    d.access(0x8b3, 1, 7, illegal = true)
    assert(d.raw(0x8b3) == 0)
    d.mode(0)
    d.inject(BigInt(1) << 8, 0x80001636L, allOnes, 7)
    assert(d.access(0x342) == 8)
    assert(d.access(0x341) == 0x80001636L)
    assert(d.access(0x343) == 0)
  }

  it should "select FDI payload before auxiliary address flags in real trap events" in {
    val root = java.nio.file.Paths.get(sys.props("e02.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    implicit val p: Parameters = parameters(enabled = true)
    test(new FDITrapEventHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("trap-event-classification"))) { dut =>
      dut.io.valid.poke(true.B)
      dut.io.interrupt.poke(false.B)
      dut.io.fetchMalTval.poke(BigInt("deadbeefdeadbeee", 16).U)
      val value = BigInt("fedcba9876543211", 16)
      dut.io.tval.poke(value.U)
      for (cause <- Seq(24, 25); flags <- Seq(false, true); translation <- Seq(0, 8, 9);
        doubleTrap <- Seq(false, true)) {
        val pc = translation match {
          case 8 => BigInt("4000001236", 16)
          case 9 => BigInt("800000001236", 16)
          case _ => BigInt("80001236", 16)
        }
        val expectedPc = if (translation == 0) pc else {
          val bits = if (translation == 8) 39 else 48
          pc | (allOnes ^ ((BigInt(1) << bits) - 1))
        }
        dut.io.cause.poke(cause.U)
        dut.io.pc.poke(pc.U)
        dut.io.translation.poke(translation.U)
        dut.io.auxiliaryFlags.poke(flags.B)
        dut.io.doubleTrap.poke(doubleTrap.B)
        for (index <- 0 until 2) {
          dut.io.validOut(index).expect(true.B)
          dut.io.epc(index).expect(expectedPc.U)
          dut.io.value(index).expect(value.U)
        }
        dut.io.causeOut(0).expect((if (doubleTrap) 16 else cause).U)
        dut.io.causeOut(1).expect(cause.U)
        dut.io.mtval2.expect((if (doubleTrap) cause else 0).U)
      }
      dut.io.auxiliaryFlags.poke(false.B)
      dut.io.doubleTrap.poke(false.B)
      dut.io.interrupt.poke(true.B)
      for (cause <- Seq(24, 25)) {
        dut.io.cause.poke(cause.U)
        for (index <- 0 until 2) {
          dut.io.value(index).expect(0.U)
          dut.io.causeOut(index).expect(((BigInt(1) << 63) | cause).U)
        }
      }
      dut.io.interrupt.poke(false.B)
      dut.io.cause.poke(1.U)
      dut.io.auxiliaryFlags.poke(true.B)
      for (index <- 0 until 2) {
        dut.io.validOut(index).expect(true.B)
        dut.io.epc(index).expect(BigInt("deadbeefdeadbeee", 16).U)
        dut.io.value(index).expect(BigInt("deadbeefdeadbeee", 16).U)
        dut.io.causeOut(index).expect(1.U)
      }
      dut.io.valid.poke(false.B)
      dut.io.validOut.foreach(_.expect(false.B))
      println("FDI trap event PASS classifications=48 interruptCases=2 fetchFlagCases=1 modules=production")
    }
  }
}
