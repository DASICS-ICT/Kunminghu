// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.decode.{Imm_Z, DecodeUnit}
import xiangshan.backend.fu.NewCSR.CSREvents.{HUEntryOutcome, InterruptEventIdentity}
import xiangshan.backend.fu.wrapper.CSR

// Observation ports expose existing production IO without adding architectural state or overrides.
class UserTimerEntryReturnProbe(implicit p: Parameters) extends CSR(FuConfig.CsrCfg) {
  val observed = IO(Output(new Bundle {
    val rawRead = UInt(64.W)
    val mode = UInt(2.W)
    val virtualMode = Bool()
    val debugMode = Bool()
    val targetTval = UInt(64.W)
    val handler = Bool()
    val entryEffect = Bool()
    val returnEffect = Bool()
    val returnRedirect = Bool()
    val returnTarget = new xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle
    val aiaRequest = Bool()
    val aiaWrite = Bool()
  }))
  observed.targetTval := trapTvalMod.io.tval
  observed.handler := csrMod.io.status.userInHandler.getOrElse(false.B)
  observed.entryEffect := csrMod.io.status.userEntryEffect.getOrElse(false.B)
  observed.returnEffect := csrMod.io.status.userReturnEffect.getOrElse(false.B)
  observed.returnRedirect := csrMod.io.out.bits.userReturnRedirect
  observed.returnTarget := csrMod.io.out.bits.userReturnTarget
  observed.rawRead := csrMod.io.out.bits.regOut
  observed.mode := csrMod.io.status.privState.PRVM.asUInt
  observed.virtualMode := csrMod.io.status.privState.V.asUInt.asBool
  observed.debugMode := csrMod.io.status.debugMode
  observed.aiaRequest := csrMod.toAIA.addr.valid
  observed.aiaWrite := csrMod.toAIA.wdata.valid
}

class UserTimerEntryReturnHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new Bundle {
      val instruction = UInt(32.W)
      val source = UInt(64.W)
    }))
    val response = Decoupled(new Bundle {
      val data = UInt(64.W)
      val illegal = Bool()
      val virtualIllegal = Bool()
    })
    val entry = Flipped(Decoupled(UInt(64.W)))
    val entryCancel = Input(Bool())
    val entryCompletion = Decoupled(new xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle)
    val entryCanceled = Output(Bool())
    val entryTerminal = Output(Valid(UInt(2.W)))
    val handler = Output(Bool())
    val entryEffect = Output(Bool())
    val returnEffect = Output(Bool())
    val returnRedirect = Output(Bool())
    val returnTarget = Output(new xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle)
    val decodeIllegal = Output(Bool())
    val noSpecExec = Output(Bool())
    val blockBackward = Output(Bool())
    val redirectValid = Output(Bool())
    val redirectTarget = Output(UInt(64.W))
    val redirectIPF = Output(Bool())
    val redirectIAF = Output(Bool())
    val redirectIGPF = Output(Bool())
    val redirectRob = Output(UInt(8.W))
    val redirectFtq = Output(UInt(8.W))
    val redirectOffset = Output(UInt(8.W))
    val traceXRet = Output(Bool())
    val requestFtq = Input(UInt(8.W))
    val requestOffset = Input(UInt(8.W))
    val fetchFault = Input(Bool())
    val fetchPageFault = Input(Bool())
    val fetchPC = Input(UInt(64.W))
    val requestRob = Input(UInt(8.W))
    val flushRob = Input(UInt(8.W))
    val targetTval = Output(UInt(64.W))
    val flush = Input(Bool())
    val trap = Input(Bool())
    val debugTrap = Input(Bool())
    val rawRead = Output(UInt(64.W))
    val mode = Output(UInt(2.W))
    val virtualMode = Output(Bool())
    val debugMode = Output(Bool())
    val aiaRequest = Output(Bool())
    val aiaWrite = Output(Bool())
  })

  val csr = Module(new UserTimerEntryReturnProbe)
  val redirectTargetWidth = csr.io.out.bits.res.redirect.get.bits.cfiUpdate.target.getWidth
  def tieInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(tieInputs)
    case vector: Vec[_] => vector.foreach(tieInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  tieInputs(csr.io)
  val decoder = Module(new DecodeUnit)
  decoder.io.enq.ctrlFlow := 0.U.asTypeOf(decoder.io.enq.ctrlFlow)
  decoder.io.enq.vtype := 0.U.asTypeOf(decoder.io.enq.vtype)
  decoder.io.enq.vstart := 0.U
  decoder.io.csrCtrl := 0.U.asTypeOf(decoder.io.csrCtrl)
  decoder.io.fromCSR := 0.U.asTypeOf(decoder.io.fromCSR)
  decoder.io.enq.ctrlFlow.instr := io.request.bits.instruction
  val decoded = decoder.io.deq.decodedInst
  io.decodeIllegal := decoded.exceptionVec(ExceptionNO.illegalInstr)
  io.noSpecExec := decoded.waitForward
  io.blockBackward := decoded.blockBackward
  io.entry.ready := false.B
  io.entryCompletion.valid := false.B
  io.entryCompletion.bits := 0.U.asTypeOf(io.entryCompletion.bits)
  io.entryCanceled := false.B
  io.entryTerminal := 0.U.asTypeOf(io.entryTerminal)
  csr.huEntry.foreach { port =>
    // This fixture drives the production reservation and PC handoff separately.
    // Captured data survives cancellation until a terminal response is consumed.
    val reserved = RegInit(false.B)
    val pcPending = RegInit(false.B)
    val savedPC = Reg(UInt(64.W))
    val savedEvent = Reg(new InterruptEventIdentity)
    val event = WireDefault(0.U.asTypeOf(new InterruptEventIdentity))
    event.interrupt.cause := 4.U
    event.interrupt.irToHU := true.B
    event.interrupt.isInterrupt := true.B
    event.robIdx.value := io.requestRob
    event.ftqIdx.value := io.requestFtq
    event.ftqOffset := io.requestOffset

    port.reserve.valid := io.entry.valid && !reserved && !io.entryCancel
    port.reserve.bits := event
    io.entry.ready := port.reserve.ready && !reserved && !io.entryCancel
    csr.io.csrio.get.userTimerDelivery.get.accepted.valid := port.reserve.fire
    csr.io.csrio.get.userTimerDelivery.get.accepted.bits := event
    port.request.valid := pcPending
    port.request.bits.event := savedEvent
    port.request.bits.pc := savedPC
    port.cancel.valid := io.entryCancel && reserved
    port.cancel.bits.event := savedEvent
    port.cancel.bits.externalRedirect := false.B

    val entering = port.completion.bits.outcome === HUEntryOutcome.enter
    io.entryCompletion.valid := port.completion.valid && entering
    io.entryCompletion.bits := port.completion.bits.target
    // Replay and trap completions terminate canceled reservations independently
    // of the backpressure applied to successful HU entry in the existing tests.
    port.completion.ready := Mux(entering, io.entryCompletion.ready, true.B)
    io.entryTerminal.valid := port.completion.fire
    io.entryTerminal.bits := port.completion.bits.outcome
    port.release := RegNext(port.completion.fire, false.B)
    io.entryCanceled := port.canceled

    when(port.reserve.fire) {
      reserved := true.B
      pcPending := true.B
      savedPC := io.entry.bits
      savedEvent := event
    }
    when(port.request.fire) {
      pcPending := false.B
    }
    when(port.completion.valid) {
      assert(reserved && !pcPending)
      assert(port.completion.bits.event.asUInt === savedEvent.asUInt)
      when(port.completion.bits.outcome === HUEntryOutcome.replay) {
        assert(port.completion.bits.target.pc === savedPC)
        assert(!port.completion.bits.target.raiseIPF)
        assert(!port.completion.bits.target.raiseIAF)
        assert(!port.completion.bits.target.raiseIGPF)
      }
    }
    when(port.release) {
      reserved := false.B
    }
  }
  io.targetTval := csr.observed.targetTval
  io.handler := csr.observed.handler
  io.entryEffect := csr.observed.entryEffect
  io.returnEffect := csr.observed.returnEffect
  io.returnRedirect := csr.observed.returnRedirect
  io.returnTarget := csr.observed.returnTarget
  csr.io.in.valid := io.request.valid
  csr.io.in.bits.ctrl.fuOpType := decoded.fuOpType
  val redirect = csr.io.out.bits.res.redirect.get
  io.redirectValid := redirect.valid
  io.redirectTarget := redirect.bits.cfiUpdate.target
  io.redirectIPF := redirect.bits.cfiUpdate.backendIPF
  io.redirectIAF := redirect.bits.cfiUpdate.backendIAF
  io.redirectIGPF := redirect.bits.cfiUpdate.backendIGPF
  io.redirectRob := redirect.bits.robIdx.value
  io.redirectFtq := redirect.bits.ftqIdx.value
  io.redirectOffset := redirect.bits.ftqOffset
  io.traceXRet := csr.io.csrio.get.isXRet
  csr.io.in.bits.ctrl.ftqIdx.get.value := io.requestFtq
  csr.io.in.bits.ctrl.ftqOffset.get := io.requestOffset
  csr.io.in.bits.ctrl.robIdx.value := io.requestRob
  csr.io.flush.bits.robIdx.value := io.flushRob
  csr.io.in.bits.data.imm := Imm_Z().minBitsFromInstr(io.request.bits.instruction)
  csr.io.in.bits.data.src(0) := io.request.bits.source
  io.request.ready := csr.io.in.ready
  csr.io.out.ready := io.response.ready
  io.response.valid := csr.io.out.valid
  io.response.bits.data := csr.io.out.bits.res.data
  val decodeIllegalSaved = RegEnable(io.decodeIllegal, csr.io.in.fire)
  io.response.bits.illegal := decodeIllegalSaved || csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
  io.response.bits.virtualIllegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.virtualInstr)
  csr.io.flush.valid := io.flush
  csr.io.flush.bits.level := RedirectLevel.flush
  csr.io.csrio.get.exception.valid := io.trap || io.debugTrap || io.fetchFault
  csr.io.csrio.get.exception.bits.pc := Mux(io.fetchFault, io.fetchPC, "h80001000".U)
  csr.io.csrio.get.exception.bits.isFetchMalAddr := io.fetchFault
  csr.io.csrio.get.exception.bits.exceptionVec(ExceptionNO.instrAccessFault) := io.fetchFault && !io.fetchPageFault
  csr.io.csrio.get.exception.bits.exceptionVec(ExceptionNO.instrPageFault) := io.fetchFault && io.fetchPageFault
  csr.io.csrio.get.exception.bits.instr := "h00000073".U
  csr.io.csrio.get.exception.bits.exceptionVec(ExceptionNO.ecallU) := io.trap
  csr.io.csrio.get.exception.bits.singleStep := io.debugTrap
  io.rawRead := csr.observed.rawRead
  io.mode := csr.observed.mode
  io.virtualMode := csr.observed.virtualMode
  io.debugMode := csr.observed.debugMode
  io.aiaRequest := csr.observed.aiaRequest
  io.aiaWrite := csr.observed.aiaWrite
}

class UserTimerEntryReturnTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "User timer real entry and return"

  private def run(name: String, enabled: Boolean = true)(body: Driver => Unit): Unit = {
    val runRoot = java.nio.file.Paths.get(sys.props("uit02.runRoot")).toRealPath()
    assert(java.nio.file.Paths.get("").toRealPath() == runRoot)
    val base = new top.DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
    test(new UserTimerEntryReturnHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation(name)
    )) { dut =>
      val d = new Driver(dut)
      d.reset()
      body(d)
      d.finishEntryTerminals()
      println(s"UserTimer entry return PASS: group=$name cycles=${d.cycles} transactions=${d.transactions} " +
        s"entries=${d.entries} returns=${d.returns} reservations=${d.entryReservations} " +
        s"enterTerminals=${d.entryTerminals} replayTerminals=${d.replayTerminals} trapTerminals=${d.trapTerminals} " +
        s"decoder=production wrapper=production backend=verilator")
    }
  }

  private class Driver(val dut: UserTimerEntryReturnHarness) {
    var cycles = 0
    var transactions = 0
    var entries = 0
    var returns = 0
    var entryReservations = 0
    var entryTerminals = 0
    var replayTerminals = 0
    var trapTerminals = 0
    val uret = BigInt("00200073", 16)
    val vector = BigInt("80002000", 16)
    val precisePC = BigInt("80001236", 16)

    def edge(): Unit = {
      if (dut.io.entry.valid.peek().litToBoolean && dut.io.entry.ready.peek().litToBoolean) {
        entryReservations += 1
      }
      if (dut.io.entryEffect.peek().litToBoolean) entries += 1
      if (dut.io.returnEffect.peek().litToBoolean) returns += 1
      if (dut.io.entryTerminal.valid.peek().litToBoolean) {
        dut.io.entryTerminal.bits.peek().litValue.toInt match {
          case 0 => entryTerminals += 1
          case 1 => replayTerminals += 1
          case 2 => trapTerminals += 1
          case other => fail(s"Unexpected HU terminal outcome: $other")
        }
      }
      assert(entryTerminals == entries, "HU state effects must match successful terminal handshakes")
      assert(entryTerminals + replayTerminals + trapTerminals <= entryReservations,
        "A reservation cannot produce more than one terminal handshake")
      dut.clock.step()
      cycles += 1
    }
    def finishEntryTerminals(): Unit = {
      var waited = 0
      while (entryTerminals + replayTerminals + trapTerminals < entryReservations && waited < 16) {
        edge()
        waited += 1
      }
      assert(entryTerminals + replayTerminals + trapTerminals == entryReservations,
        "Every reservation must terminate or be discarded by an explicit reset")
    }
    def idle(n: Int): Unit = (0 until n).foreach(_ => edge())
    def reset(): Unit = {
      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.instruction.poke(0x73.U)
      dut.io.request.bits.source.poke(0.U)
      dut.io.response.ready.poke(true.B)
      dut.io.entry.valid.poke(false.B)
      dut.io.entry.bits.poke(precisePC.U)
      dut.io.entryCancel.poke(false.B)
      dut.io.entryCompletion.ready.poke(true.B)
      dut.io.fetchFault.poke(false.B)
      dut.io.fetchPageFault.poke(false.B)
      dut.io.fetchPC.poke(0.U)
      dut.io.requestFtq.poke(0.U)
      dut.io.requestOffset.poke(0.U)
      dut.io.requestRob.poke(0.U)
      dut.io.flushRob.poke(0.U)
      dut.io.flush.poke(false.B)
      dut.io.trap.poke(false.B)
      dut.io.debugTrap.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      entries = 0
      returns = 0
      entryReservations = 0
      entryTerminals = 0
      replayTerminals = 0
      trapTerminals = 0
      edge()
      dut.io.mode.expect(3.U)
      dut.io.handler.expect(false.B)
    }
    def raw(addr: Int): BigInt = {
      dut.io.request.valid.expect(false.B)
      dut.io.request.bits.instruction.poke(instruction(addr).U)
      dut.io.rawRead.peek().litValue
    }
    def fetchFault(pc: BigInt, page: Boolean, delegated: Boolean, handler: Boolean): Unit = {
      dut.io.fetchPC.poke(pc.U)
      dut.io.fetchPageFault.poke(page.B)
      dut.io.fetchFault.poke(true.B)
      edge()
      dut.io.fetchFault.poke(false.B)
      edge()
      dut.io.mode.expect((if (delegated) 1 else 3).U)
      dut.io.handler.expect(handler.B)
      assert(read(if (delegated) 0x142 else 0x342) == (if (page) 12 else 1))
      assert(read(if (delegated) 0x143 else 0x343) == pc)
      assert(read(if (delegated) 0x141 else 0x341) == (pc & ~BigInt(1)))
    }
    def instruction(addr: Int, write: Boolean = false): BigInt =
      (BigInt(addr) << 20) | (if (write) BigInt(1) << 15 else BigInt(0)) |
        ((if (write) 1 else 2) << 12) | (1 << 7) | 0x73

    def begin(inst: BigInt, source: BigInt = 0, ready: Boolean = true): Unit = {
      dut.io.request.bits.instruction.poke(inst.U)
      dut.io.request.bits.source.poke(source.U)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke(ready.B)
      var waited = 0
      while (!dut.io.request.ready.peek().litToBoolean && waited < 16) {
        edge()
        waited += 1
      }
      dut.io.request.ready.expect(true.B)
      edge()
      transactions += 1
      dut.io.request.valid.poke(false.B)
    }
    def access(addr: Int, value: Option[BigInt] = None, illegal: Boolean = false): BigInt = {
      begin(instruction(addr, value.nonEmpty), value.getOrElse(BigInt(0)))
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(illegal.B)
      dut.io.response.bits.virtualIllegal.expect(false.B)
      val result = dut.io.response.bits.data.peek().litValue
      edge()
      dut.io.response.valid.expect(false.B)
      result
    }
    def read(addr: Int, illegal: Boolean = false): BigInt = access(addr, illegal = illegal)
    def write(addr: Int, value: BigInt): Unit = { access(addr, Some(value)); () }
    def system(inst: BigInt, illegal: Boolean = false): Unit = {
      begin(inst)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(illegal.B)
      dut.io.response.bits.virtualIllegal.expect(false.B)
      if (illegal) {
        dut.io.returnRedirect.expect(false.B)
        dut.io.redirectValid.expect(false.B)
        dut.io.traceXRet.expect(false.B)
        dut.io.returnEffect.expect(false.B)
      }
      edge()
    }
    def mode(privilege: Int, virtual: Boolean = false): Unit = {
      dut.io.mode.expect(3.U)
      write(0x300, (BigInt(privilege) << 11) | (if (virtual) BigInt(1) << 39 else BigInt(0)))
      write(0x341, precisePC)
      system(BigInt("30200073", 16))
      dut.io.mode.expect(privilege.U)
      dut.io.virtualMode.expect(virtual.B)
    }
    def machine(): Unit = {
      dut.io.trap.poke(true.B)
      edge()
      dut.io.trap.poke(false.B)
      edge()
      dut.io.mode.expect(3.U)
      dut.io.virtualMode.expect(false.B)
    }
    def debug(): Unit = {
      dut.io.debugTrap.poke(true.B)
      edge()
      dut.io.debugTrap.poke(false.B)
      dut.io.debugMode.expect(true.B)
    }
    def enable(): Unit = {
      write(0x30c, (BigInt(1) << 63) | 1)
      write(0x60c, (BigInt(1) << 63) | 1)
      write(0x10c, 1)
    }
    def prepare(target: BigInt = vector): Unit = {
      enable()
      write(0x005, target)
      write(0x000, 1)
      write(0x004, 0x10)
      mode(0)
      write(0x800, 1)
      idle(1)
      assert(read(0x044) == 0x10)
    }
    def beginEntry(pc: BigInt = precisePC, ready: Boolean = true): Unit = {
      dut.io.entry.bits.poke(pc.U)
      dut.io.entry.valid.poke(true.B)
      dut.io.entryCompletion.ready.poke(ready.B)
      dut.io.entry.ready.expect(true.B)
      dut.io.entryEffect.expect(false.B)
      edge()
      dut.io.entry.valid.poke(false.B)
      var waited = 0
      while (!dut.io.entryCompletion.valid.peek().litToBoolean && waited < 16) {
        dut.io.entryEffect.expect(false.B)
        dut.io.handler.expect(false.B)
        edge()
        waited += 1
      }
      dut.io.entryCompletion.valid.expect(true.B)
      dut.io.handler.expect(false.B)
    }
    def entry(pc: BigInt = precisePC, target: BigInt = vector): Unit = {
      val before = entries
      beginEntry(pc)
      dut.io.entryCompletion.bits.pc.expect(target.U)
      dut.io.entryEffect.expect(true.B)
      edge()
      assert(entries == before + 1)
      dut.io.handler.expect(true.B)
      dut.io.entryCompletion.valid.expect(false.B)
      assert(read(0x041) == (pc & ~BigInt(1)))
      assert(read(0x000) == 0x10)
      assert(read(0x042) == ((BigInt(1) << 63) | 4))
      assert(read(0x043) == 0)
      assert(read(0x044) == 0)
    }
    def returnUser(target: BigInt = precisePC, stalls: Int = 0): Unit = {
      val before = returns
      val savedBank = Seq(0x000, 0x041, 0x042, 0x043, 0x044).map(a => a -> raw(a))
      dut.io.request.bits.instruction.poke(uret.U)
      dut.io.decodeIllegal.expect(false.B)
      dut.io.noSpecExec.expect(true.B)
      dut.io.blockBackward.expect(true.B)
      begin(uret, ready = stalls == 0)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(false.B)
      dut.io.returnTarget.pc.expect(target.U)
      dut.io.redirectValid.expect(true.B)
      dut.io.redirectTarget.expect(target.U)
      for (_ <- 0 until stalls) {
        dut.io.returnEffect.expect(false.B)
        dut.io.traceXRet.expect(false.B)
        dut.io.redirectValid.expect(true.B)
        dut.io.redirectTarget.expect(target.U)
        dut.io.handler.expect(true.B)
        dut.io.returnTarget.pc.expect(target.U)
        savedBank.foreach { case (addr, value) => assert(raw(addr) == value) }
        edge()
      }
      dut.io.response.ready.poke(true.B)
      dut.io.returnEffect.expect(true.B)
      dut.io.returnRedirect.expect(true.B)
      dut.io.redirectValid.expect(true.B)
      dut.io.traceXRet.expect(true.B)
      edge()
      assert(returns == before + 1)
      dut.io.handler.expect(false.B)
    }
  }

  it should "commit one precise HU entry from a saved request through backpressure" in run("precise-entry") { d =>
    d.prepare()
    val otherBanks = Seq(0x300, 0x341, 0x342, 0x343, 0x100, 0x141, 0x142, 0x143,
      0x200, 0x241, 0x242, 0x243, 0x7b0, 0x7b1).map(a => a -> d.raw(a)).toMap
    d.beginEntry(ready = false)
    d.dut.io.entry.bits.poke(BigInt("deadbeef00000000", 16).U)
    d.dut.io.request.bits.instruction.poke(d.instruction(0x341).U)
    for (_ <- 0 until 7) {
      d.dut.io.entryCompletion.bits.pc.expect(d.vector.U)
      d.dut.io.entryEffect.expect(false.B)
      d.dut.io.handler.expect(false.B)
      assert(d.raw(0x041) == 0)
      assert(d.raw(0x000) == 1)
      assert(d.raw(0x042) == 0)
      assert(d.raw(0x043) == 0)
      assert(d.raw(0x044) == 0x10)
      d.edge()
    }
    assert(d.entries == 0)
    d.dut.io.entryCompletion.ready.poke(true.B)
    d.edge()
    d.dut.io.handler.expect(true.B)
    assert(d.entries == 1)
    otherBanks.foreach { case (addr, value) => assert(d.raw(addr) == value, s"other bank changed: $addr") }
    assert(d.read(0x041) == d.precisePC)
    assert(d.read(0x042) == ((BigInt(1) << 63) | 4))
    assert(d.read(0x043) == 0)
    assert(d.read(0x000) == 0x10)
    assert(d.read(0x044) == 0)
    d.write(0x041, 0x1234)
    d.write(0x042, 7)
    d.write(0x043, 9)
    d.write(0x000, 0)
    d.idle(5)
    assert(d.read(0x041) == 0x1234)
    assert(d.read(0x042) == 7)
    assert(d.read(0x043) == 9)
    assert(d.read(0x000) == 0)
    assert(d.entries == 1 && d.returns == 0)
  }

  it should "execute real decoded URET once and preserve the saved target while stalled" in run("decoded-uret") { d =>
    d.prepare()
    d.entry()
    d.write(0x800, 19)
    d.returnUser(stalls = 6)
    assert(d.read(0x800) == 19)
    assert(d.read(0x000) == 0x11)
    assert(d.entries == 1 && d.returns == 1)
    d.system(d.uret, illegal = true)
    d.write(0x000, 0)
    d.idle(4)
    assert(d.read(0x000) == 0)
    assert(d.returns == 1)
  }

  for (enabled <- Seq(false, true)) {
    it should s"reject URET in every illegal source and debug with enabled=$enabled" in
      run(s"uret-denial-$enabled", enabled) { d =>
        for ((priv, virtual) <- Seq((3, false), (1, false), (0, false), (1, true), (0, true))) {
          d.reset()
          d.enable()
          d.mode(priv, virtual)
          d.system(d.uret, illegal = true)
          assert(d.entries == 0 && d.returns == 0)
          d.dut.io.handler.expect(false.B)
        }
        d.reset()
        d.debug()
        d.system(d.uret, illegal = true)
        d.dut.io.debugMode.expect(true.B)
        assert(d.returns == 0)
      }
  }

  it should "cancel HU entry before acceptance and completion without consuming pending" in run("entry-cancel") { d =>
    d.prepare()
    d.dut.io.entry.valid.poke(true.B)
    d.dut.io.entryCancel.poke(true.B)
    d.dut.io.entry.ready.expect(false.B)
    d.edge()
    d.dut.io.entry.valid.poke(false.B)
    d.dut.io.entryCancel.poke(false.B)
    assert(d.read(0x044) == 0x10)
    d.beginEntry(ready = false)
    d.idle(3)
    d.dut.io.entryCancel.poke(true.B)
    d.dut.io.entryCanceled.expect(true.B)
    d.dut.io.entryCompletion.valid.expect(false.B)
    d.edge()
    d.dut.io.entryCancel.poke(false.B)
    d.dut.io.handler.expect(false.B)
    assert(d.read(0x041) == 0)
    assert(d.read(0x044) == 0x10)
    assert(d.entries == 0)
    d.entry()
  }

  it should "cancel uncommitted entry and return for existing traps and flush" in run("event-priority") { d =>
    d.prepare()
    d.beginEntry(ready = false)
    d.machine()
    assert(d.entries == 0)
    d.dut.io.handler.expect(false.B)
    assert(d.read(0x044) == 0x10)
    d.mode(0)
    d.entry()
    d.begin(d.uret, ready = false)
    d.dut.io.flush.poke(true.B)
    d.dut.io.returnEffect.expect(false.B)
    d.edge()
    d.dut.io.flush.poke(false.B)
    d.dut.io.response.ready.poke(true.B)
    d.dut.io.handler.expect(true.B)
    assert(d.returns == 0)
    d.returnUser()
    assert(d.returns == 1)
  }

  it should "retain the accepted URET identity across younger flush and competing input" in run("return-identity") { d =>
    d.prepare()
    d.entry()
    d.dut.io.requestRob.poke(4.U)
    d.dut.io.requestFtq.poke(7.U)
    d.dut.io.requestOffset.poke(2.U)
    d.begin(d.uret, ready = false)
    d.dut.io.flushRob.poke(8.U)
    d.dut.io.flush.poke(true.B)
    d.dut.io.requestRob.poke(12.U)
    d.dut.io.requestFtq.poke(19.U)
    d.dut.io.requestOffset.poke(3.U)
    d.dut.io.request.bits.instruction.poke(d.instruction(0x040, write = true).U)
    d.dut.io.request.bits.source.poke(0xbad.U)
    d.dut.io.request.valid.poke(true.B)
    d.dut.io.request.ready.expect(false.B)
    d.dut.io.response.valid.expect(true.B)
    d.dut.io.returnTarget.pc.expect(d.precisePC.U)
    d.edge()
    d.dut.io.flush.poke(false.B)
    d.dut.io.request.ready.expect(false.B)
    d.edge()
    d.dut.io.flush.poke(true.B)
    d.dut.io.response.valid.expect(true.B)
    d.dut.io.returnTarget.pc.expect(d.precisePC.U)
    d.edge()
    d.dut.io.flush.poke(false.B)
    d.dut.io.request.valid.poke(false.B)
    d.dut.io.response.ready.poke(true.B)
    d.dut.io.returnEffect.expect(true.B)
    d.dut.io.redirectValid.expect(true.B)
    d.dut.io.redirectRob.expect(4.U)
    d.dut.io.redirectFtq.expect(7.U)
    d.dut.io.redirectOffset.expect(2.U)
    d.dut.io.redirectTarget.expect(d.precisePC.U)
    d.edge()
    assert(d.returns == 1)
    assert(d.read(0x040) == 0)
  }

  it should "give trap and debug priority over ready user completion" in run("ready-event-priority") { d =>
    for (debug <- Seq(false, true)) {
      d.reset()
      d.prepare()
      d.beginEntry(ready = false)
      d.dut.io.entryCompletion.ready.poke(true.B)
      if (debug) d.dut.io.debugTrap.poke(true.B) else d.dut.io.trap.poke(true.B)
      d.dut.io.entryEffect.expect(false.B)
      d.dut.io.entryCompletion.valid.expect(false.B)
      d.edge()
      d.dut.io.trap.poke(false.B)
      d.dut.io.debugTrap.poke(false.B)
      d.dut.io.handler.expect(false.B)
      assert(d.entries == 0)
    }
    for (debug <- Seq(false, true)) {
      d.reset()
      d.prepare()
      d.entry()
      d.begin(d.uret, ready = false)
      d.dut.io.response.ready.poke(true.B)
      if (debug) d.dut.io.debugTrap.poke(true.B) else d.dut.io.trap.poke(true.B)
      d.dut.io.returnEffect.expect(false.B)
      d.edge()
      d.dut.io.trap.poke(false.B)
      d.dut.io.debugTrap.poke(false.B)
      d.dut.io.handler.expect(true.B)
      assert(d.returns == 0)
    }
  }

  it should "freeze reloaded handler time through machine and debug preemption and revoked stateen" in run("handler-lifecycle") { d =>
    d.prepare()
    d.entry()
    d.write(0x800, 23)
    d.write(0x000, 0x11)
    d.dut.io.entry.valid.poke(true.B)
    d.dut.io.entry.ready.expect(false.B)
    d.idle(4)
    d.dut.io.entry.valid.poke(false.B)
    assert(d.read(0x800) == 23)
    d.machine()
    d.dut.io.handler.expect(true.B)
    assert(d.read(0x800) == 23)
    d.mode(0)
    d.debug()
    d.idle(4)
    d.dut.io.handler.expect(true.B)
    d.system(BigInt("7b200073", 16))
    d.dut.io.mode.expect(0.U)
    assert(d.read(0x800) == 23)
    d.machine()
    d.write(0x30c, BigInt(1) << 63)
    d.mode(0)
    d.read(0x800, illegal = true)
    d.returnUser()
    assert(d.returns == 1)
    d.read(0x800, illegal = true)
    d.machine()
    assert(d.read(0x800) == 20)
  }

  it should "arbitrate software writes before HU and reject live software while HU owns the slot" in run("software-entry-arbitration") { d =>
    d.prepare()
    d.dut.io.entry.valid.poke(true.B)
    d.dut.io.entry.bits.poke(d.precisePC.U)
    d.dut.io.request.bits.instruction.poke(d.instruction(0x040, write = true).U)
    d.dut.io.request.bits.source.poke(0x1234.U)
    d.dut.io.request.valid.poke(true.B)
    d.dut.io.entry.ready.expect(false.B)
    d.dut.io.request.ready.expect(true.B)
    d.edge()
    d.transactions += 1
    d.dut.io.request.valid.poke(false.B)
    d.dut.io.entry.ready.expect(false.B)
    d.edge()
    d.dut.io.entryCompletion.ready.poke(false.B)
    d.dut.io.entry.ready.expect(true.B)
    d.edge()
    d.dut.io.entry.valid.poke(false.B)
    for (addr <- Seq(0x040, 0x340, 0x351)) {
      d.dut.io.request.valid.poke(true.B)
      d.dut.io.request.bits.instruction.poke(d.instruction(addr, write = true).U)
      d.dut.io.request.bits.source.poke(0xbad.U)
      d.dut.io.request.ready.expect(false.B)
      d.dut.io.aiaRequest.expect(false.B)
      d.dut.io.aiaWrite.expect(false.B)
      d.dut.io.entryEffect.expect(false.B)
      d.edge()
    }
    d.dut.io.request.bits.instruction.poke(BigInt("30200073", 16).U)
    d.dut.io.request.ready.expect(false.B)
    d.dut.io.traceXRet.expect(false.B)
    d.edge()
    d.dut.io.request.valid.poke(false.B)
    d.dut.io.entryCompletion.ready.poke(true.B)
    d.edge()
    assert(d.entries == 1)
    assert(d.read(0x040) == 0x1234)
    d.machine()
    assert(d.read(0x340) == 0)
  }

  it should "repeat exact entry return pairs with zero UPIE and preserve handler across HS SRET" in run("repeated-entry-hs") { d =>
    d.prepare()
    for (iteration <- 0 until 3) {
      val pc = d.precisePC + iteration * 8
      d.entry(pc)
      d.write(0x000, 0)
      d.returnUser(pc)
      assert(d.read(0x000) == 0x10)
      assert(d.read(0x041) == pc)
      assert(d.entries == iteration + 1 && d.returns == iteration + 1)
      d.write(0x000, 1)
      d.write(0x800, 1)
      d.idle(1)
    }
    d.entry()
    d.write(0x800, 31)
    d.machine()
    d.write(0x302, BigInt(1) << ExceptionNO.ecallU)
    d.mode(0)
    d.dut.io.trap.poke(true.B)
    d.edge()
    d.dut.io.trap.poke(false.B)
    d.edge()
    d.dut.io.mode.expect(1.U)
    d.dut.io.handler.expect(true.B)
    assert(d.read(0x800) == 31)
    d.system(BigInt("10200073", 16))
    d.dut.io.mode.expect(0.U)
    d.dut.io.handler.expect(true.B)
    assert(d.read(0x800) == 31)
    d.returnUser()
    assert(d.entries == 4 && d.returns == 4)
  }

  it should "discard a C0 flushed URET and reset outstanding entry and return" in run("flush-reset-boundaries") { d =>
    d.prepare()
    d.entry()
    d.dut.io.flush.poke(true.B)
    d.begin(d.uret)
    d.dut.io.returnEffect.expect(false.B)
    d.edge()
    d.dut.io.flush.poke(false.B)
    d.dut.io.handler.expect(true.B)
    assert(d.returns == 0)
    d.begin(d.uret, ready = false)
    d.reset()
    d.dut.io.response.valid.expect(false.B)
    d.dut.io.handler.expect(false.B)
    assert(d.read(0x000) == 0)
    assert(d.read(0x041) == 0)
    d.prepare()
    d.beginEntry(ready = false)
    d.reset()
    d.dut.io.entryCompletion.valid.expect(false.B)
    d.dut.io.handler.expect(false.B)
    assert(d.read(0x044) == 0)
    assert(d.read(0x041) == 0)
  }

  it should "attribute actual bad-target fetch traps to M or HS without restoring handler state" in run("fetch-fault-attribution") { d =>
    for (delegated <- Seq(false, true); returning <- Seq(false, true); page <- Seq(false, true)) {
      d.reset()
      val bad = if (page) BigInt(1) << 39 else BigInt(1) << 63
      if (page) d.write(0x180, BigInt(8) << 60)
      if (delegated) d.write(0x302, BigInt(1) << (if (page) 12 else 1))
      d.prepare(target = if (returning) d.vector else bad)
      d.beginEntry()
      d.edge()
      if (returning) {
        d.write(0x041, bad)
        d.begin(d.uret)
        d.dut.io.redirectValid.expect(true.B)
        // The wrapper target is virtual-address-width; full XLEN tval is saved separately.
        d.dut.io.redirectTarget.expect((bad & ((BigInt(1) << d.dut.redirectTargetWidth) - 1)).U)
        d.dut.io.redirectIPF.expect(page.B)
        d.dut.io.redirectIAF.expect((!page).B)
        d.dut.io.redirectIGPF.expect(false.B)
        d.edge()
      }
      d.dut.io.targetTval.expect(bad.U)
      d.fetchFault(bad, page, delegated, handler = !returning)
      assert(d.entries == 1 && d.returns == (if (returning) 1 else 0))
    }
  }

  it should "carry bad Bare and Sv39 targets as fetch faults without rolling back entry or URET" in run("target-faults") { d =>
    for ((satp, bad, ipf, iaf) <- Seq(
      (BigInt(0), BigInt(1) << 63, false, true),
      (BigInt(8) << 60, BigInt(1) << 39, true, false),
      (BigInt(9) << 60, BigInt(1) << 48, true, false))) {
      d.reset()
      d.write(0x180, satp)
      d.prepare(target = bad)
      d.beginEntry()
      d.dut.io.entryCompletion.bits.pc.expect(bad.U)
      d.dut.io.entryCompletion.bits.raiseIPF.expect(ipf.B)
      d.dut.io.entryCompletion.bits.raiseIAF.expect(iaf.B)
      d.dut.io.entryCompletion.bits.raiseIGPF.expect(false.B)
      d.edge()
      d.dut.io.handler.expect(true.B)
      d.write(0x041, bad)
      d.begin(d.uret)
      d.dut.io.returnTarget.pc.expect(bad.U)
      d.dut.io.returnTarget.raiseIPF.expect(ipf.B)
      d.dut.io.returnTarget.raiseIAF.expect(iaf.B)
      d.dut.io.returnTarget.raiseIGPF.expect(false.B)
      d.edge()
      d.dut.io.handler.expect(false.B)
      assert(d.entries == 1 && d.returns == 1)
      d.dut.io.targetTval.expect(bad.U)
      d.machine()
      d.prepare(target = bad | 0x1000)
      d.beginEntry(ready = false)
      d.idle(2)
      d.dut.io.targetTval.expect(bad.U)
      d.dut.io.entryCancel.poke(true.B)
      d.edge()
      d.dut.io.entryCancel.poke(false.B)
      d.dut.io.targetTval.expect(bad.U)
      d.dut.io.handler.expect(false.B)
      assert(d.entries == 1)
    }
  }
  it should "validate final HU entry guards with privilege enables debug STEP and NMIE" in run("entry-eligibility") { d =>
    def rejected(): Unit = {
      val before = d.entries
      d.dut.io.entry.valid.poke(true.B)
      d.dut.io.entry.bits.poke(d.precisePC.U)
      for (_ <- 0 until 3) {
        d.dut.io.entry.ready.expect(false.B)
        d.dut.io.entryCompletion.valid.expect(false.B)
        d.dut.io.entryEffect.expect(false.B)
        d.edge()
      }
      d.dut.io.entry.valid.poke(false.B)
      d.dut.io.handler.expect(false.B)
      assert(d.entries == before)
    }

    d.enable()
    d.write(0x005, d.vector)
    d.write(0x000, 1)
    d.write(0x004, 0x10)
    d.mode(0)
    rejected()
    d.write(0x800, 1)
    d.idle(8)
    assert(d.read(0x044) == 0x10)
    d.dut.io.handler.expect(false.B)
    d.dut.io.entryCompletion.valid.expect(false.B)
    assert(d.entries == 0)
    d.write(0x000, 0)
    rejected()
    d.write(0x000, 1)
    d.write(0x004, 0)
    rejected()
    d.write(0x004, 0x10)
    d.entry()

    for ((privilege, virtual) <- Seq((3, false), (1, false), (1, true), (0, true))) {
      d.reset()
      d.prepare()
      d.machine()
      d.mode(privilege, virtual)
      rejected()
    }

    for (upperBlocked <- Seq(false, true)) {
      d.reset()
      d.prepare()
      d.machine()
      if (upperBlocked) {
        d.write(0x30c, BigInt(1) << 63)
        // The stored supervisor enable remains set, but its effective read is masked by M.
        assert(d.read(0x10c) == 0)
      } else {
        d.write(0x10c, 0)
      }
      d.mode(0)
      rejected()
      d.machine()
      if (upperBlocked) d.write(0x30c, (BigInt(1) << 63) | 1)
      else d.write(0x10c, 1)
      d.mode(0)
      d.entry()
    }

    d.reset()
    d.prepare()
    d.debug()
    rejected()
    val savedDcsr = d.read(0x7b0)
    d.write(0x7b0, savedDcsr | 4)
    d.system(BigInt("7b200073", 16))
    d.dut.io.debugMode.expect(false.B)
    d.dut.io.mode.expect(0.U)
    rejected()

    d.reset()
    d.prepare()
    d.machine()
    // This implementation explicitly permits software to clear NMIE for testing.
    d.write(0x744, 0)
    assert((d.read(0x744) & 8) == 0)
    d.mode(0)
    rejected()
  }

  it should "validate final URET guards in preempted handler contexts" in run("preempted-return-guards") { d =>
    for ((privilege, virtual, debug) <- Seq((3, false, false), (1, false, false),
      (1, true, false), (0, true, false), (0, false, true))) {
      d.reset()
      d.prepare()
      d.entry()
      d.write(0x800, 13)
      if (debug) d.debug()
      else { d.machine(); d.mode(privilege, virtual) }
      d.begin(d.uret, ready = false)
      for (_ <- 0 until 3) {
        d.dut.io.response.valid.expect(true.B)
        d.dut.io.response.bits.illegal.expect(true.B)
        d.dut.io.response.bits.virtualIllegal.expect(false.B)
        d.dut.io.redirectValid.expect(false.B)
        d.dut.io.returnEffect.expect(false.B)
        d.dut.io.traceXRet.expect(false.B)
        d.dut.io.handler.expect(true.B)
        d.edge()
      }
      d.dut.io.response.ready.poke(true.B)
      d.edge()
      d.dut.io.handler.expect(true.B)
      assert(d.raw(0x041) == d.precisePC)
      assert(d.raw(0x000) == 0x10)
      assert(d.raw(0x800) == 13)
      assert(d.entries == 1 && d.returns == 0)
    }
  }

}
