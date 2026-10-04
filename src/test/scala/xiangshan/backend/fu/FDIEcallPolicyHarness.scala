// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility.ClockGate
import xiangshan._
import xiangshan.backend.Bundles.ExceptionInfo
import xiangshan.backend.decode.Imm_Z
import xiangshan.backend.exu.ExeUnit
import xiangshan.backend.fu.NewCSR.FDIFReasonModule
import xiangshan.backend.fu.NewCSR.CSREvents.InterruptEventIdentity
import xiangshan.backend.fu.wrapper.CSR
import xiangshan.backend.rob.{ExceptionGen, RobPtr}
import xiangshan.frontend.FtqPtr

object FDIEcallPolicyAddresses {
  val native = Seq(0x141, 0x142, 0x143, 0x341, 0x342, 0x343, 0x34a, 0x64a,
    0x34b, 0x300, 0x600, 0x741, 0x742, 0x744, 0x7b0, 0x7b1)
}

class FDIEcallPolicyIO(implicit p: Parameters) extends XSBundle {
  val request = Flipped(Decoupled(new Bundle {
    val instruction = UInt(32.W)
    val operation = UInt(6.W)
    val operand = UInt(64.W)
    val basePc = UInt(VAddrBits.W)
    val ftq = new FtqPtr
    val offset = UInt(log2Ceil(PredictWidth).W)
    val rob = new RobPtr
    val pdest = UInt(8.W)
    val notTrusted = Bool()
  }))
  val response = Decoupled(new Bundle {
    val data = UInt(64.W)
    val vector = UInt(26.W)
    val tval = UInt(64.W)
    val reason = UInt(3.W)
    val rob = new RobPtr
    val pdest = UInt(8.W)
    val flushPipe = Bool()
  })
  val redirect = Input(Valid(new Redirect))
  val trap = Input(Valid(new ExceptionInfo))
  val useSelected = Input(Bool())
  val trackResponse = Input(Bool())
  val competitor = Input(Valid(new Bundle {
    val rob = new RobPtr
    val vector = UInt(26.W)
  }))
  val selected = Output(Valid(new Bundle {
    val rob = new RobPtr
    val vector = UInt(26.W)
    val tval = UInt(64.W)
    val reason = UInt(3.W)
  }))
  val state = Output(Vec(52, UInt(64.W)))
  val writes = Output(UInt(52.W))
  val nativeState = Output(Vec(FDIEcallPolicyAddresses.native.size, UInt(64.W)))
  val mode = Output(UInt(2.W))
  val virtualMode = Output(Bool())
  val debugMode = Output(Bool())
  val distribution = Output(new DistributedCSRIO)
  val softwareReason = Output(Bool())
  val hardwareReason = Output(Bool())
  val hardwareReasonValue = Output(UInt(3.W))
  val fdiDistribution = Output(Bool())
  val wrapperFire = Output(Bool())
  val bankFire = Output(Bool())
  val wrapperValid = Output(Bool())
  val faultInstruction = Output(Valid(new Bundle {
    val operation = UInt(6.W)
    val imm = UInt(Imm_Z().len.W)
    val ftq = new FtqPtr
    val offset = UInt(log2Ceil(PredictWidth).W)
  }))
  val savedInstruction = Output(Valid(UInt(32.W)))
  val savedFtq = Output(new FtqPtr)
  val savedOffset = Output(UInt(log2Ceil(PredictWidth).W))
  val transportedTrap = Output(Bool())
  val machineEntry = Output(Bool())
  val supervisorEntry = Output(Bool())
  val nmiSource = Input(Bool())
  val acceptNmi = Input(Bool())
  val useSavedNmi = Input(Bool())
  val nmiCandidate = Output(Bool())
  val resetClockEnable = Output(Bool())
  val monitorReset = Input(Bool())
  val gatedResetEdges = Output(UInt(8.W))
}

object FDIEcallPolicyWiring {
  def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }

  // This wiring only observes existing owners. The software driver establishes
  // privilege and configuration using the same CSR input as the ECALL under test.
  def observations(csr: CSR, io: FDIEcallPolicyIO)(implicit p: Parameters): Unit = {
    val bank = csr.csrMod
    FDICSRTestAddresses.all.zipWithIndex.foreach { case (address, index) =>
      io.state(index) := bank.csrOutMap.get(address).map(observe(_)).getOrElse(0.U)
    }
    io.writes := VecInit(FDICSRTestAddresses.all.map(address =>
      bank.csrRwMap.get(address).map(entry => observe(entry._1.wen)).getOrElse(false.B))).asUInt
    FDIEcallPolicyAddresses.native.zipWithIndex.foreach { case (address, index) =>
      io.nativeState(index) := bank.csrOutMap.get(address).map(observe(_)).getOrElse(0.U)
    }
    io.mode := observe(bank.io.status.privState.PRVM).asUInt
    io.virtualMode := observe(bank.io.status.privState.V).asUInt.asBool
    io.debugMode := observe(bank.io.status.debugMode)
    io.distribution := observe(csr.io.csrio.get.customCtrl.distribute_csr)
    io.softwareReason := bank.csrRwMap.get(0x8b3).map(entry => observe(entry._1.wen)).getOrElse(false.B)
    val owner = bank.csrMods.find(_.addr == 0x8b3).map(_.asInstanceOf[FDIFReasonModule])
    io.hardwareReason := owner.map(module => observe(module.trapReason.valid)).getOrElse(false.B)
    io.hardwareReasonValue := owner.map(module => observe(module.trapReason.bits.REASON).asUInt).getOrElse(0.U)
    io.fdiDistribution := bank.io.distributedFDI.map(port => observe(port.w.valid)).getOrElse(false.B)
    io.wrapperFire := observe(csr.io.in.valid) && observe(csr.io.in.ready)
    io.bankFire := observe(bank.io.in.valid) && observe(bank.io.in.ready)
    io.wrapperValid := observe(csr.io.out.valid)
    val fault = csr.trapInstMod.io.faultCsrUop
    io.faultInstruction.valid := observe(fault.valid)
    io.faultInstruction.bits.operation := observe(fault.bits.fuOpType)
    io.faultInstruction.bits.imm := observe(fault.bits.imm)
    io.faultInstruction.bits.ftq := observe(fault.bits.ftqInfo.ftqPtr)
    io.faultInstruction.bits.offset := observe(fault.bits.ftqInfo.ftqOffset)
    io.savedInstruction := observe(csr.trapInstMod.io.currentTrapInst)
    io.savedFtq := observe(csr.trapInstMod.trapInstInfo.ftqPtr)
    io.savedOffset := observe(csr.trapInstMod.trapInstInfo.ftqOffset)
    io.transportedTrap := observe(csr.io.csrio.get.exception.valid)
    io.machineEntry := observe(bank.trapEntryMEvent.valid)
    io.supervisorEntry := observe(bank.trapEntryHSEvent.valid)
  }

  def nmiInputs(port: CSRFileIO, io: FDIEcallPolicyIO)(implicit p: Parameters): Unit = {
    io.nmiCandidate := false.B
    port.externalInterrupt.nmi.nmi_31 := io.nmiSource
    port.userTimerDelivery.foreach { delivery =>
      val identity = WireInit(0.U.asTypeOf(new InterruptEventIdentity))
      identity.interrupt := delivery.candidate.bits
      identity.robIdx := io.request.bits.rob
      val canClaim = delivery.candidate.valid && delivery.candidate.bits.nmi
      val claim = io.acceptNmi && canClaim
      val saved = RegEnable(identity, claim)
      delivery.accepted.valid := claim
      delivery.accepted.bits := identity
      io.nmiCandidate := canClaim
      when(io.acceptNmi) { assert(canClaim) }
      when(io.useSavedNmi) { port.exception.bits.interruptEvent.get := saved }
    }
  }
}

class FDIEcallPolicyHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new FDIEcallPolicyIO)
  val csr = Module(new CSR(FuConfig.CsrCfg))
  FDIEcallPolicyWiring.idleInputs(csr.io)
  csr.huEntry.foreach { entry => FDIEcallPolicyWiring.idleInputs(entry); entry.completion.ready := true.B }
  val request = io.request.bits
  csr.io.in.valid := io.request.valid
  csr.io.in.bits.ctrl.fuOpType := request.operation
  csr.io.in.bits.ctrl.robIdx := request.rob
  csr.io.in.bits.ctrl.pdest := request.pdest
  csr.io.in.bits.ctrl.ftqIdx.get := request.ftq
  csr.io.in.bits.ctrl.ftqOffset.get := request.offset
  csr.io.in.bits.ctrl.fdiNotTrusted.foreach(_ := request.notTrusted)
  csr.io.in.bits.data.pc.get := request.basePc
  csr.io.in.bits.data.imm := Imm_Z().minBitsFromInstr(request.instruction)
  csr.io.in.bits.data.src(0) := request.operand
  io.request.ready := csr.io.in.ready
  csr.io.out.ready := io.response.ready
  io.response.valid := csr.io.out.valid
  io.response.bits.data := csr.io.out.bits.res.data
  io.response.bits.vector := csr.io.out.bits.ctrl.exceptionVec.get.asUInt
  io.response.bits.tval := csr.io.out.bits.ctrl.fdiException.map(_.tval).getOrElse(0.U)
  io.response.bits.reason := csr.io.out.bits.ctrl.fdiException.map(_.reason).getOrElse(0.U)
  io.response.bits.rob := csr.io.out.bits.ctrl.robIdx
  io.response.bits.pdest := csr.io.out.bits.ctrl.pdest
  io.response.bits.flushPipe := csr.io.out.bits.ctrl.flushPipe.get
  csr.io.flush := io.redirect
  csr.io.csrio.get.exception := io.trap
  FDIEcallPolicyWiring.nmiInputs(csr.io.csrio.get, io)
  io.selected := 0.U.asTypeOf(io.selected)
  io.resetClockEnable := false.B
  io.gatedResetEdges := 0.U
  FDIEcallPolicyWiring.observations(csr, io)
}

// Only the accepted ROB boundary and source-PC lookup are supplied by the test.
// ECALL generation, exception age selection and CSR trap transport are production logic.
class FDIEcallPolicyTrapHarness(implicit p: Parameters) extends LazyModule {
  private val backend = p(XSCoreParamsKey).backendParams
  backend.allSchdParams.foreach(_.bindBackendParam(backend))
  backend.allIssueParams.foreach(_.bindBackendParam(backend))
  backend.allExuParams.zipWithIndex.foreach { case (exu, index) =>
    exu.bindBackendParam(backend)
    exu.updateIQWakeUpConfigs(backend.iqWakeUpParams)
    exu.updateExuIdx(index)
  }
  val csrUnit = LazyModule(new ExeUnit(backend.allExuParams.find(_.hasCSR).get))
  lazy val module = new FDIEcallPolicyTrapHarnessImp(this)

  class FDIEcallPolicyTrapHarnessImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasXSParameter {
    val io = IO(new FDIEcallPolicyIO)
    val exu = csrUnit.module
    val selector = Module(new ExceptionGen(backend))
    FDIEcallPolicyWiring.idleInputs(exu.io)
    exu.io.csrio.get.userTimerDelivery.foreach(_.entry.completion.ready := true.B)
    val request = io.request.bits
    exu.io.in.valid := io.request.valid
    exu.io.in.bits.fuType := FuType.csr.U
    exu.io.in.bits.fuOpType := request.operation
    exu.io.in.bits.robIdx := request.rob
    exu.io.in.bits.pdest := request.pdest
    exu.io.in.bits.ftqIdx.foreach(_ := request.ftq)
    exu.io.in.bits.ftqOffset.foreach(_ := request.offset)
    exu.io.in.bits.fdiNotTrusted.foreach(_ := request.notTrusted)
    exu.io.in.bits.pc.foreach(_ := request.basePc)
    exu.io.in.bits.imm := Imm_Z().minBitsFromInstr(request.instruction)
    exu.io.in.bits.src(0) := request.operand
    exu.io.in.bits.rfWen.foreach(_ := true.B)
    io.request.ready := exu.io.in.ready
    exu.io.out.ready := io.response.ready
    io.response.valid := exu.io.out.valid
    io.response.bits.data := exu.io.out.bits.data.head
    io.response.bits.vector := exu.io.out.bits.exceptionVec.get.asUInt
    io.response.bits.tval := exu.io.out.bits.fdiException.map(_.tval).getOrElse(0.U)
    io.response.bits.reason := exu.io.out.bits.fdiException.map(_.reason).getOrElse(0.U)
    io.response.bits.rob := exu.io.out.bits.robIdx
    io.response.bits.pdest := exu.io.out.bits.pdest
    io.response.bits.flushPipe := exu.io.out.bits.flushPipe.get
    exu.io.flush := io.redirect

    selector.io.redirect := io.redirect
    selector.io.enq.foreach { port => port.valid := false.B; port.bits := 0.U.asTypeOf(port.bits) }
    selector.io.wb.foreach { port => port.valid := false.B; port.bits := 0.U.asTypeOf(port.bits) }
    val ports = backend.allExuParams.filter(_.exceptionOut.nonEmpty)
    val csrPort = ports.indexWhere(_.hasCSR)
    val loadPort = ports.indexWhere(_.fuConfigs.exists(_.name == "ldu"))
    require(csrPort >= 0 && loadPort >= 0 && csrPort != loadPort)
    val wb = selector.io.wb(csrPort)
    wb.valid := exu.io.out.fire && io.trackResponse && exu.io.out.bits.exceptionVec.get.asUInt.orR
    wb.bits.robIdx := exu.io.out.bits.robIdx
    wb.bits.exceptionVec := ExceptionNO.partialSelect(exu.io.out.bits.exceptionVec.get, ports(csrPort).exceptionOut)
    wb.bits.hasException := wb.bits.exceptionVec.asUInt.orR
    wb.bits.fdiException.foreach(_ := exu.io.out.bits.fdiException.get)
    val competitor = selector.io.wb(loadPort)
    competitor.valid := io.competitor.valid
    competitor.bits.robIdx := io.competitor.bits.rob
    competitor.bits.exceptionVec := ExceptionNO.partialSelect(io.competitor.bits.vector.asTypeOf(ExceptionVec()),
      ports(loadPort).exceptionOut)
    competitor.bits.hasException := competitor.bits.exceptionVec.asUInt.orR
    io.selected.valid := selector.io.state.valid
    io.selected.bits.rob := selector.io.state.bits.robIdx
    io.selected.bits.vector := selector.io.state.bits.exceptionVec.asUInt
    io.selected.bits.tval := selector.io.state.bits.fdiException.map(_.tval).getOrElse(0.U)
    io.selected.bits.reason := selector.io.state.bits.fdiException.map(_.reason).getOrElse(0.U)
    val accept = io.trap.valid && io.useSelected
    when(accept) { assert(selector.io.state.valid) }
    selector.io.flush := accept
    val captured = WireInit(io.trap.bits)
    when(io.useSelected) {
      captured.exceptionVec := selector.io.state.bits.exceptionVec
      captured.fdiException.foreach(_ := selector.io.state.bits.fdiException.get)
    }
    exu.io.csrio.get.exception.valid := RegNext(io.trap.valid, false.B)
    exu.io.csrio.get.exception.bits := RegEnable(captured, io.trap.valid)
    FDIEcallPolicyWiring.nmiInputs(exu.io.csrio.get, io)

    val csr = exu.funcUnits.collectFirst { case unit: CSR => unit }.get
    FDIEcallPolicyWiring.observations(csr, io)
    val clockTest = ClockGate.genTeSrc
    clockTest.cgen := reset.asBool
    io.resetClockEnable := clockTest.cgen
    val divider = exu.funcUnits.find(_.cfg.name == "div").get
    val dividerClock = observe(divider.clock)
    io.gatedResetEdges := withClockAndReset(dividerClock, io.monitorReset.asAsyncReset) {
      val edges = RegInit(0.U(8.W))
      when(reset.asBool) { edges := edges + 1.U }
      edges
    }
  }
}
