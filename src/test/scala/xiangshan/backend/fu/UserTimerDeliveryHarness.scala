// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.{Config, Parameters}
import xiangshan._
import xiangshan.backend.{Backend, CtrlToFtqIO}
import xiangshan.backend.Bundles.ExceptionInfo
import xiangshan.backend.fu.NewCSR.UserTimerCSRAddress
import xiangshan.backend.fu.NewCSR.CSREvents.{HUEntryCancel, HUEntryCompletion, HUEntryRequest, InterruptDescriptor, InterruptEventIdentity}
import xiangshan.backend.fu.wrapper.CSR
import xiangshan.backend.fu.FuConfig.VialuCfg
import xiangshan.backend.fu.vector.Bundles.VConfig
import xiangshan.frontend._

// These test configurations keep the production core shape and change only this feature.
class UserTimerEnabledFpgaConfig(n: Int = 1) extends Config(
  (new top.FpgaDefaultConfig(n)).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(_.copy(HasUserTimerInterrupt = true))
  })
)

object UserTimerDeliveryParameters {
  def apply(enabled: Boolean): Parameters = {
    // The production parser supplies logging/performance parameter keys used by the complete backend.
    val (base, _, _) = top.ArgParser.parse(Array(
      "--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768",
      "--fpga-platform", "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    val core = base(XSTileKey).head.copy(HasUserTimerInterrupt = enabled)
    require(core.HasVPU && core.VLEN == 128)
    require(base(DebugOptionsKey).FPGAPlatform && !base(DebugOptionsKey).EnableDifftest &&
      !base(DebugOptionsKey).AlwaysBasicDiff && !base(DebugOptionsKey).EnableChiselDB)
    base.alterPartial {
      case XSCoreParamsKey => core
    }
  }
}

// The fixture supplies the interfaces normally owned by IFU/BPU and MemBlock.
// Decode, rename, scheduling, register files, ROB and CSR writeback remain production logic.
class UserTimerDeliveryHarness(implicit p: Parameters) extends XSModule {
  require(HasVPU && VLEN == 128)
  val backendOuter = LazyModule(new Backend(backendParams))
  // Production passes an asynchronous reset into these modules, including idle gated units.
  val backend = withReset(reset.asAsyncReset) { Module(backendOuter.module) }
  val ftq = withReset(reset.asAsyncReset) { Module(new Ftq) }

  val vialuUnits = backendOuter.inner.vfExuBlock.get.exus.flatMap(
    _.module.funcUnits.filter(_.cfg == VialuCfg))
  val debugEntryAddress = p(freechips.rocketchip.devices.debug.DebugModuleKey).get.baseAddress + 0x800

  val io = IO(new Bundle {
    val instruction = Flipped(Decoupled(new CtrlFlow))
    val instructionTail = Vec(DecodeWidth - 1, Flipped(Decoupled(new CtrlFlow)))
    val bpu = Flipped(new BpuToFtqIO)
    val predecode = Input(Valid(new PredecodeWritebackBundle))
    val fetchReady = Input(Bool())
    val traceEnable = Input(Bool())
    val traceStall = Input(Bool())
    val interrupts = new ExternalInterruptIO
    val backendRedirect = Output(Valid(new Redirect))
    val frontendRedirect = Output(Valid(new BranchPredictionRedirect))
    val ifuRedirect = Output(Valid(new BranchPredictionRedirect))
    val ftqToBackend = Output(new FtqToCtrlIO)
    val ftqNext = Output(new FtqPtr)
    val fetchRequest = Output(Valid(new FetchRequestBundle))
    val commits = Output(new CtrlToFtqIO)
    val robFlush = Output(Valid(new Redirect))
    val robException = Output(Valid(new ExceptionInfo))
    val mode = Output(UInt(2.W))
    val virtualMode = Output(Bool())
    val debugMode = Output(Bool())
    val handler = Output(Bool())
    val entryEffect = Output(Bool())
    val returnEffect = Output(Bool())
    val interruptSelected = Output(Bool())
    val timerRemaining = Output(UInt(64.W))
    val timerPending = Output(Bool())
    val timerTick = Output(Bool())
    val timerWrite = Output(Valid(UInt(64.W)))
    val timerConsume = Output(Bool())
    val ustatus = Output(UInt(64.W))
    val uie = Output(UInt(64.W))
    val utvec = Output(UInt(64.W))
    val uepc = Output(UInt(64.W))
    val ucause = Output(UInt(64.W))
    val utval = Output(UInt(64.W))
    val csrRequest = Output(Valid(UInt(12.W)))
    val csrRequestRob = Output(new xiangshan.backend.rob.RobPtr)
    val csrExeRequest = Output(Valid(new xiangshan.backend.rob.RobPtr))
    val csrSiblingReady = Output(Vec(backendParams.allExuParams.find(_.hasCSR).get.fuConfigs.size, Bool()))
    val csrResponse = Output(Valid(UInt(64.W)))
    val candidate = Output(Valid(new InterruptDescriptor))
    val candidateKill = Output(Bool())
    val accepted = Output(Valid(new InterruptEventIdentity))
    val huReserve = Output(Valid(new InterruptEventIdentity))
    val huReserveReady = Output(Bool())
    val huRequestAtControl = Output(Valid(new HUEntryRequest))
    val huRequest = Output(Valid(new HUEntryRequest))
    val huRequestReady = Output(Bool())
    val huCancel = Output(Valid(new HUEntryCancel))
    val huCompletion = Output(Valid(new HUEntryCompletion))
    val huCompletionReady = Output(Bool())
    val huRelease = Output(Bool())
    val filterCandidateStages = Output(Vec(6, Valid(new InterruptDescriptor)))
    val robCandidate = Output(Valid(new InterruptDescriptor))
    val robHeadValid = Output(Bool())
    val robHeadInterruptSafe = Output(Bool())
    val legalCSRWrite = Output(Valid(UInt(12.W)))
    val userQualifierWrites = Output(Vec(3, Valid(UInt(64.W))))
    val ordinaryTrap = Output(Bool())
    val mnPending = Output(UInt(64.W))
    val mnEntry = Output(Bool())
    val mnepc = Output(UInt(64.W))
    val mncause = Output(UInt(64.W))
    val robEnq = Output(Vec(RenameWidth, Valid(new Bundle {
      val robIdx = new xiangshan.backend.rob.RobPtr
      val ftqIdx = new FtqPtr
      val ftqOffset = UInt(log2Ceil(PredictWidth).W)
      val first = Bool()
      val last = Bool()
      val uopIdx = UInt(log2Ceil(MaxUopSize + 1).W)
      val numWB = UInt(log2Ceil(MaxUopSize + 1).W)
      val instrSize = UInt(log2Ceil(RenameWidth + 1).W)
      val fused = Bool()
    })))
    val robHead = Output(new Bundle {
      val valid = Bool()
      val robIdx = new xiangshan.backend.rob.RobPtr
      val interruptSafe = Bool()
      val sealedGroup = Bool()
      val writebacked = Bool()
      val needFlush = Bool()
    })
    val robRetire = Output(Vec(CommitWidth, Valid(new xiangshan.backend.rob.RobPtr)))
    val vectorIssue = Output(Vec(vialuUnits.size, Valid(new Bundle {
      val robIdx = new xiangshan.backend.rob.RobPtr
      val vuopIdx = UInt(log2Ceil(MaxUopSize + 1).W)
      val vl = UInt(8.W)
      val vstart = UInt(8.W)
      val vsew = UInt(2.W)
      val vlmul = UInt(3.W)
    })))
    val vectorWB = Output(Vec(vialuUnits.size, Valid(new Bundle {
      val robIdx = new xiangshan.backend.rob.RobPtr
      val vuopIdx = UInt(log2Ceil(MaxUopSize + 1).W)
      val data = UInt(128.W)
      val vxsat = Bool()
    })))
    val fpFlags = Output(UInt(5.W))
    val vecSat = Output(Bool())
    val vecStart = Output(UInt(64.W))
    val userScratch = Output(UInt(64.W))
    val icacheFault = Output(Valid(new FtqPtr))
    val prefetchFault = Output(Valid(new Bundle {
      val ftqIdx = new FtqPtr
      val kind = UInt(xiangshan.frontend.ExceptionType.width.W)
    }))
    val mEntry = Output(Bool())
    val hsEntry = Output(Bool())
    val vsEntry = Output(Bool())
    val debugEntry = Output(Bool())
    val mepc = Output(UInt(64.W))
    val mcause = Output(UInt(64.W))
    val mtval = Output(UInt(64.W))
    val sepc = Output(UInt(64.W))
    val scause = Output(UInt(64.W))
    val vsepc = Output(UInt(64.W))
    val vscause = Output(UInt(64.W))
    val mnstatus = Output(UInt(64.W))
    val dpc = Output(UInt(64.W))
    val dcsr = Output(UInt(64.W))
    val huEffectLocked = Output(Bool())
    val cumulativeHuEffects = Output(UInt(64.W))
    val cumulativeConsumes = Output(UInt(64.W))
    val cumulativeCompletions = Output(UInt(64.W))
    val cumulativeBackendRedirects = Output(UInt(64.W))
    val cumulativeDebugEntries = Output(UInt(64.W))
    val cumulativeQualifiedTicks = Output(UInt(64.W))
    val robCommitStuck = Output(UInt(21.W))
    val criticalErrorState = Output(Bool())
    val fusionEnabled = Output(Bool())
    val scalarRename = Output(Vec(RenameWidth, Valid(new Bundle {
      val inputFtqIdx = new FtqPtr
      val inputOffset = UInt(log2Ceil(PredictWidth).W)
      val robIdx = new xiangshan.backend.rob.RobPtr
      val outputOffset = UInt(log2Ceil(PredictWidth).W)
      val instrSize = UInt(log2Ceil(RenameWidth + 1).W)
      val first = Bool()
      val last = Bool()
      val canCompress = Bool()
      val needsRob = Bool()
      val fused = Bool()
    })))
    val scalarFusion = Output(Vec(RenameWidth - 1, Valid(new Bundle {
      val ftqIdx = new FtqPtr
      val ftqOffset = UInt(log2Ceil(PredictWidth).W)
      val clearNext = Bool()
    })))
    val scalarWriteback = Output(Vec(backendParams.genWrite2CtrlBundles.length,
      Valid(new xiangshan.backend.rob.RobPtr)))
    val watchdogOverflow = Output(Bool())
    val robWatchdogError = Output(Bool())
    val controlWatchdogError = Output(Bool())
    val csrCriticalInput = Output(Bool())
    val criticalDebugDeferred = Output(Bool())
    val huCanceled = Output(Bool())
    val cumulativeHuReservations = Output(UInt(64.W))
    val cumulativeHuRequests = Output(UInt(64.W))
    val cumulativeHuReleases = Output(UInt(64.W))
    val cumulativeRobRetireCycles = Output(UInt(64.W))
    val traceBlocked = Output(Bool())
    val traceInput = Output(new xiangshan.backend.trace.TraceBundle(false, CommitWidth, IretireWidthInPipe))
    val traceHeld = Output(new xiangshan.backend.trace.TraceBundle(false, CommitWidth, IretireWidthInPipe))
    val traceEncoder = Output(new xiangshan.backend.trace.TraceBundle(false, TraceGroupNum, IretireWidthCompressed))
    val traceCause = Output(UInt(CauseWidth.W))
    val traceTval = Output(UInt(TvalWidth.W))
    val tracePrivilege = Output(UInt(PrivWidth.W))
    val traceAddresses = Output(Vec(TraceGroupNum, UInt(IaddrWidth.W)))
    val traceOffsets = Output(Vec(TraceGroupNum, UInt(log2Ceil(PredictWidth).W)))
  })

  def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  idleInputs(backend.io)
  idleInputs(ftq.io)
  backend.io.frontend.cfVec.head <> io.instruction
  backend.io.frontend.cfVec.drop(1).zip(io.instructionTail).foreach { case (sink, source) => sink <> source }
  backend.io.frontend.fromFtq := ftq.io.toBackend
  ftq.io.fromBackend := backend.io.frontend.toFtq
  ftq.io.fromBpu <> io.bpu
  ftq.io.fromIfu.pdWb := io.predecode
  ftq.io.toIfu.req.ready := io.fetchReady
  ftq.io.toICache.req.ready := io.fetchReady
  ftq.io.toPrefetch.req.ready := true.B
  backend.io.mem.lqCanAccept := true.B
  backend.io.mem.sqCanAccept := true.B
  backend.io.fenceio.sbuffer.sbIsEmpty := true.B
  backend.io.traceCoreInterface.fromEncoder.enable := io.traceEnable
  backend.io.traceCoreInterface.fromEncoder.stall := io.traceStall
  backend.io.fromTop.externalInterrupt := io.interrupts
  io.backendRedirect := backend.io.frontend.toFtq.redirect
  io.frontendRedirect := ftq.io.toBpu.redirect
  io.ifuRedirect := ftq.io.toIfu.redirect
  io.ftqToBackend := ftq.io.toBackend
  io.ftqNext := ftq.io.toBpu.enq_ptr
  io.fetchRequest.valid := ftq.io.toIfu.req.valid
  io.fetchRequest.bits := ftq.io.toIfu.req.bits
  io.commits := backend.io.frontend.toFtq

  val ctrl = backendOuter.inner.ctrlBlock.module
  val rob = backendOuter.inner.ctrlBlock.rob.module
  val csrExu = backendOuter.inner.intExuBlock.get.exus.find(_.exuParams.hasCSR).get.module
  val csr = csrExu.funcUnits.collectFirst { case unit: CSR => unit }.get
  val csrMod = csr.csrMod

  // Read-only taps do not bypass normal requests, qualification or retirement.
  io.filterCandidateStages := 0.U.asTypeOf(io.filterCandidateStages)
  csrMod.intrMod.candidateStages.foreach(stages => io.filterCandidateStages := observe(stages))
  io.robCandidate.valid := observe(rob.intrBitSetReg)
  io.robCandidate.bits := rob.interruptDescriptorReg.map(observe(_))
    .getOrElse(0.U.asTypeOf(new InterruptDescriptor))
  io.robHeadValid := observe(rob.deqPtrEntryValid)
  io.robHeadInterruptSafe := observe(rob.deqPtrEntry.interrupt_safe)
  io.legalCSRWrite.valid := observe(csrMod.io.in.valid) && observe(csrMod.io.in.ready) &&
    observe(csrMod.permitMod.io.out.hasLegalWen) && !observe(csrMod.io.in.bits.redirectFlush)
  io.legalCSRWrite.bits := observe(csrMod.io.in.bits.addr)
  io.userQualifierWrites := 0.U.asTypeOf(io.userQualifierWrites)
  Seq(UserTimerCSRAddress.ustatus, UserTimerCSRAddress.uie, UserTimerCSRAddress.utimer)
    .zipWithIndex.foreach { case (address, index) =>
      csrMod.userTimerCSRMap.get(address).foreach { case (write, _) =>
        io.userQualifierWrites(index).valid := observe(write.wen)
        io.userQualifierWrites(index).bits := observe(write.wdata)
      }
    }
  io.ordinaryTrap := observe(csrMod.hasTrap)
  io.mnPending := observe(csrMod.nmip).asUInt
  io.mnEntry := observe(csrMod.trapEntryMNEvent.valid)
  io.mnepc := observe(csrMod.mnepc.rdata)
  io.mncause := observe(csrMod.mncause.rdata)
  io.traceBlocked := observe(ctrl.trace.io.out.blockRobCommit)
  io.traceInput := observe(ctrl.trace.io.in.fromRob)
  io.traceHeld := observe(ctrl.trace.s1_out)
  io.traceEncoder := observe(ctrl.trace.io.out.toEncoder)
  io.traceCause := backend.io.traceCoreInterface.toEncoder.trap.cause
  io.traceTval := backend.io.traceCoreInterface.toEncoder.trap.tval
  io.tracePrivilege := backend.io.traceCoreInterface.toEncoder.priv
  io.traceAddresses := VecInit(backend.io.traceCoreInterface.toEncoder.groups.map(_.bits.iaddr))
  io.traceOffsets := VecInit(backend.io.traceCoreInterface.toEncoder.groups.map(_.bits.ftqOffset.get))
  io.robFlush := observe(rob.io.flushOut)
  io.robException := observe(ctrl.io.robio.exception)
  io.mode := observe(csrMod.io.status.privState.PRVM).asUInt
  io.virtualMode := observe(csrMod.io.status.privState.V).asUInt.asBool
  io.debugMode := observe(csrMod.io.status.debugMode)
  io.handler := csrMod.io.status.userInHandler.map(observe(_)).getOrElse(false.B)
  io.entryEffect := csrMod.io.status.userEntryEffect.map(observe(_)).getOrElse(false.B)
  io.returnEffect := csrMod.io.status.userReturnEffect.map(observe(_)).getOrElse(false.B)
  io.interruptSelected := observe(csrMod.io.status.interrupt)
  io.timerRemaining := 0.U
  io.timerPending := false.B
  io.timerTick := false.B
  io.timerWrite := 0.U.asTypeOf(io.timerWrite)
  io.timerConsume := false.B
  csrMod.userTimer.foreach { timer =>
    io.timerRemaining := observe(timer.io.remaining)
    io.timerPending := observe(timer.io.pending)
    io.timerTick := observe(timer.io.tickEnable)
    io.timerWrite := observe(timer.io.write)
    io.timerConsume := observe(timer.io.consume)
  }
  def bank(address: Int): UInt =
    csrMod.userTimerCSROutMap.get(address).map(observe(_)).getOrElse(0.U(64.W))
  io.ustatus := bank(UserTimerCSRAddress.ustatus)
  io.uie := bank(UserTimerCSRAddress.uie)
  io.utvec := bank(UserTimerCSRAddress.utvec)
  io.uepc := bank(UserTimerCSRAddress.uepc)
  io.ucause := bank(UserTimerCSRAddress.ucause)
  io.utval := bank(UserTimerCSRAddress.utval)
  io.csrRequest.valid := observe(csrMod.io.in.valid) && observe(csrMod.io.in.ready)
  io.csrRequest.bits := observe(csrMod.io.in.bits.addr)
  io.csrRequestRob := observe(csr.io.in.bits.ctrl.robIdx)
  io.csrExeRequest.valid := observe(csrExu.io.in.valid) && observe(csrExu.io.in.ready)
  io.csrExeRequest.bits := observe(csrExu.io.in.bits.robIdx)
  io.csrSiblingReady := VecInit(csrExu.funcUnits.map(unit => observe(unit.io.in.ready)))
  io.csrResponse.valid := observe(csrExu.io.out.valid) && observe(csrExu.io.out.ready)
  io.csrResponse.bits := observe(csrExu.io.out.bits.data.head)
  io.candidate := 0.U.asTypeOf(io.candidate)
  io.candidateKill := false.B
  io.accepted := 0.U.asTypeOf(io.accepted)
  io.huReserve := 0.U.asTypeOf(io.huReserve)
  io.huReserveReady := false.B
  io.huRequestAtControl := 0.U.asTypeOf(io.huRequestAtControl)
  io.huRequest := 0.U.asTypeOf(io.huRequest)
  io.huRequestReady := false.B
  io.huCancel := 0.U.asTypeOf(io.huCancel)
  io.huCompletion := 0.U.asTypeOf(io.huCompletion)
  io.huCompletionReady := false.B
  io.huRelease := false.B
  ctrl.io.robio.csr.userTimerDelivery.foreach { port =>
    io.candidate := observe(port.candidate)
    io.candidateKill := observe(port.candidateKill)
    io.accepted := observe(port.accepted)
    io.huRequestAtControl.valid := observe(port.entry.request.valid)
    io.huRequestAtControl.bits := observe(port.entry.request.bits)
  }
  csr.huEntry.foreach { port =>
    io.huReserve.valid := observe(port.reserve.valid)
    io.huReserve.bits := observe(port.reserve.bits)
    io.huReserveReady := observe(port.reserve.ready)
    io.huRequest.valid := observe(port.request.valid)
    io.huRequest.bits := observe(port.request.bits)
    io.huRequestReady := observe(port.request.ready)
    io.huCancel := observe(port.cancel)
    io.huCompletion.valid := observe(port.completion.valid)
    io.huCompletion.bits := observe(port.completion.bits)
    io.huCompletionReady := observe(port.completion.ready)
    io.huRelease := observe(port.release)
  }

  io.robEnq.zipWithIndex.foreach { case (dst, i) =>
    val src = observe(rob.io.enq.req(i))
    dst.valid := src.valid && observe(rob.io.enq.canAccept)
    dst.bits.robIdx := src.bits.robIdx
    dst.bits.ftqIdx := src.bits.ftqPtr
    dst.bits.ftqOffset := src.bits.ftqOffset
    dst.bits.first := src.bits.firstUop
    dst.bits.last := src.bits.lastUop
    dst.bits.uopIdx := src.bits.uopIdx
    dst.bits.numWB := src.bits.numWB
    dst.bits.instrSize := src.bits.instrSize
    dst.bits.fused := xiangshan.CommitType.isFused(src.bits.commitType)
  }
  io.robHead.valid := observe(rob.deqPtrEntry.commit_v)
  io.robHead.robIdx := observe(rob.io.robDeqPtr)
  io.robHead.interruptSafe := observe(rob.deqPtrEntry.interrupt_safe)
  io.robHead.sealedGroup := rob.deqPtrEntry.huGroupSealed.map(observe(_)).getOrElse(false.B)
  io.robHead.writebacked := observe(rob.deqPtrEntry.commit_w)
  io.robHead.needFlush := observe(rob.deqPtrEntry.needFlush)
  io.robRetire.zipWithIndex.foreach { case (dst, i) =>
    dst.valid := observe(rob.io.commits.isCommit) && observe(rob.io.commits.commitValid(i))
    dst.bits := observe(rob.io.commits.robIdx(i))
  }
  vialuUnits.zipWithIndex.foreach { case (unit, i) =>
    val in = observe(unit.io.in.bits)
    val out = observe(unit.io.out.bits)
    val actualVConfig = in.data.getSrcVConfig.asTypeOf(new VConfig)
    io.vectorIssue(i).valid := observe(unit.io.in.valid) && observe(unit.io.in.ready)
    io.vectorIssue(i).bits.robIdx := in.ctrl.robIdx
    io.vectorIssue(i).bits.vuopIdx := in.ctrl.vpu.get.vuopIdx
    io.vectorIssue(i).bits.vl := actualVConfig.vl
    io.vectorIssue(i).bits.vstart := in.ctrl.vpu.get.vstart
    io.vectorIssue(i).bits.vsew := in.ctrl.vpu.get.vsew
    io.vectorIssue(i).bits.vlmul := in.ctrl.vpu.get.vlmul
    io.vectorWB(i).valid := observe(unit.io.out.valid) && observe(unit.io.out.ready)
    io.vectorWB(i).bits.robIdx := out.ctrl.robIdx
    io.vectorWB(i).bits.vuopIdx := out.ctrl.vpu.get.vuopIdx
    io.vectorWB(i).bits.data := out.res.data
    io.vectorWB(i).bits.vxsat := out.res.vxsat.get.asUInt.asBool
  }
  val observedFcsr = observe(csrMod.fcsr.rdata)
  val observedVcsr = observe(csrMod.vcsr.rdata)
  io.fpFlags := observedFcsr(4, 0)
  io.vecSat := observedVcsr(0)
  io.vecStart := observe(csrMod.vstart.rdata)
  io.userScratch := bank(UserTimerCSRAddress.uscratch)
  io.icacheFault.valid := ftq.io.toICache.req.valid && ftq.io.toICache.req.ready &&
    ftq.io.toICache.req.bits.backendException
  io.icacheFault.bits := ftq.io.toICache.req.bits.pcMemRead.head.ftqIdx
  io.prefetchFault.valid := ftq.io.toPrefetch.req.valid && ftq.io.toPrefetch.req.ready &&
    xiangshan.frontend.ExceptionType.hasException(ftq.io.toPrefetch.backendException)
  io.prefetchFault.bits.ftqIdx := ftq.io.toPrefetch.req.bits.ftqIdx
  io.prefetchFault.bits.kind := ftq.io.toPrefetch.backendException
  io.mEntry := observe(csrMod.trapEntryMEvent.valid)
  io.hsEntry := observe(csrMod.trapEntryHSEvent.valid)
  io.vsEntry := observe(csrMod.trapEntryVSEvent.valid)
  io.debugEntry := observe(csrMod.trapEntryDEvent.valid)
  io.mepc := observe(csrMod.mepc.rdata)
  io.mcause := observe(csrMod.mcause.rdata)
  io.mtval := observe(csrMod.mtval.rdata)
  io.sepc := observe(csrMod.sepc.rdata)
  io.scause := observe(csrMod.scause.rdata)
  io.vsepc := observe(csrMod.vsepc.rdata)
  io.vscause := observe(csrMod.vscause.rdata)
  io.mnstatus := observe(csrMod.mnstatus.rdata)
  io.dpc := observe(csrMod.dpc.rdata)
  io.dcsr := observe(csrMod.dcsr.rdata)
  io.huEffectLocked := csr.huEntry.map(port => observe(port.effectLocked)).getOrElse(false.B)
  // Observer counters retain evidence across inactive clock batches without feeding the backend.
  def eventCounter(event: Bool): UInt = {
    val count = RegInit(0.U(64.W))
    when(event) { count := count + 1.U }
    count
  }
  io.cumulativeHuEffects := eventCounter(io.entryEffect)
  io.cumulativeConsumes := eventCounter(io.timerConsume)
  io.cumulativeCompletions := eventCounter(io.huCompletion.valid && io.huCompletionReady)
  io.cumulativeBackendRedirects := eventCounter(io.backendRedirect.valid)
  io.cumulativeDebugEntries := eventCounter(io.debugEntry)
  io.cumulativeQualifiedTicks := eventCounter(io.timerTick && !io.timerWrite.valid && !io.timerConsume)
  io.robCommitStuck := observe(rob.commitStuckCycle)
  io.criticalErrorState := observe(csrMod.criticalErrorState)

  io.fusionEnabled := !observe(ctrl.fusionDecoder.io.disableFusion)
  // Observe fields separately so a Decoupled probe cannot reverse the ready connection.
  io.scalarRename.zipWithIndex.foreach { case (dst, i) =>
    val in = ctrl.rename.io.in(i)
    val out = ctrl.rename.io.out(i)
    dst.valid := observe(in.valid) && observe(in.ready)
    dst.bits.inputFtqIdx := observe(in.bits.ftqPtr)
    dst.bits.inputOffset := observe(in.bits.ftqOffset)
    dst.bits.robIdx := observe(out.bits.robIdx)
    dst.bits.outputOffset := observe(out.bits.ftqOffset)
    dst.bits.instrSize := observe(out.bits.instrSize)
    dst.bits.first := observe(out.bits.firstUop)
    dst.bits.last := observe(out.bits.lastUop)
    dst.bits.canCompress := observe(ctrl.rename.compressUnit.io.out.canCompressVec(i))
    dst.bits.needsRob := observe(ctrl.rename.compressUnit.io.out.needRobFlags(i))
    dst.bits.fused := xiangshan.CommitType.isFused(observe(out.bits.commitType))
  }
  io.scalarFusion.zipWithIndex.foreach { case (dst, i) =>
    val out = ctrl.rename.io.out(i)
    val in = ctrl.rename.io.in(i)
    dst.valid := observe(ctrl.fusionDecoder.io.out(i).valid) && observe(out.valid) && observe(out.ready)
    dst.bits.ftqIdx := observe(in.bits.ftqPtr)
    dst.bits.ftqOffset := observe(in.bits.ftqOffset)
    dst.bits.clearNext := observe(ctrl.fusionDecoder.io.clear(i + 1))
  }
  io.scalarWriteback.zipWithIndex.foreach { case (dst, i) =>
    dst.valid := observe(rob.io.exuWriteback(i).valid)
    dst.bits := observe(rob.io.exuWriteback(i).bits.robIdx)
  }
  // Timing observations follow the unchanged watchdog source through each real register boundary.
  require(rob.criticalErrors.size == 1 && rob.criticalErrors.head._1.trim == "rob_commit_stuck")
  io.watchdogOverflow := observe(rob.commitStuck_overflow)
  io.robWatchdogError := observe(rob.io_error.head)
  io.controlWatchdogError := observe(ctrl.io_error.head)
  io.csrCriticalInput := observe(csrMod.io.fromTop.criticalErrorState)
  io.criticalDebugDeferred := csrMod.intrMod.io.in.criticalDebug.map(observe(_)).getOrElse(false.B)
  io.huCanceled := csr.huEntry.map(port => observe(port.canceled)).getOrElse(false.B)
  io.cumulativeHuReservations := eventCounter(io.huReserve.valid && io.huReserveReady)
  io.cumulativeHuRequests := eventCounter(io.huRequest.valid && io.huRequestReady)
  io.cumulativeHuReleases := eventCounter(io.huRelease)
  io.cumulativeRobRetireCycles := eventCounter(io.robRetire.map(_.valid).reduce(_ || _))
}
