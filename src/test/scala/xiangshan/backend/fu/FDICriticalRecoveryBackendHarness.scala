// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.FtqPtr
import chisel3.reflect.DataMirror
import utility.{DelayN, RegNextWithEnable}
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.Bundles._

class FDICriticalSourceObservation(implicit p: Parameters) extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val instr = UInt(32.W)
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val operation = FuOpType()
  val tag = Bool()
  val waitForward = Bool()
  val blockBackward = Bool()
}

class FDICriticalJumpObservation(implicit p: Parameters) extends XSBundle {
  val inValid = Bool()
  val inReady = Bool()
  val outValid = Bool()
  val outReady = Bool()
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val basePc = UInt(XLEN.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val operation = FuOpType()
  val tag = Bool()
  val result = UInt(XLEN.W)
  val rfWen = Bool()
  val exception = UInt(ExceptionVec.ExceptionVecSize.W)
  val redirect = Valid(new Redirect)
  val flush = Valid(new Redirect)
}

class FDICriticalStorePhaseObservation(implicit p: Parameters) extends XSBundle {
  val redirect = Valid(new Redirect)
  val s0Fire = Bool()
  val s0Kill = Bool()
  val s1Valid = Bool()
  val s1Fire = Bool()
  val s1Kill = Bool()
  val s1Rob = new RobPtr
  val s1Sq = new SqPtr
  val s2Valid = Bool()
  val s2Fire = Bool()
  val s2Kill = Bool()
  val s2Rob = new RobPtr
  val s2Sq = new SqPtr
  val translation = Valid(new TlbReq)
}

// Only external frontend/interrupt inputs drive the real Backend and FTQ.
// Every additional signal below is a passive observation of the unchanged DUT.
class FDICriticalRecoveryBackendHarness(implicit p: Parameters) extends UserTimerDeliveryHarness {
  require(HasFDI && maxCommitStuck == (1 << 21))
  require(rob.commitStuckCycle.getWidth == 21)
  val jumps = backendOuter.inner.intExuBlock.get.exus.flatMap(_.module.funcUnits).filter(_.cfg.isJmp)
  require(jumps.nonEmpty)

  val monitor = IO(new Bundle {
    val renamed = Output(Vec(RenameWidth, Valid(new FDICriticalSourceObservation)))
    val jumps = Output(Vec(FDICriticalRecoveryBackendHarness.this.jumps.size, new FDICriticalJumpObservation))
    val robEmpty = Output(Bool())
    val waitForward = Output(Bool())
    val blockBackward = Output(Bool())
    val huOutstanding = Output(Bool())
    val hasTrap = Output(Bool())
    val criticalDebug = Output(Bool())
    val targetUpdate = Output(Bool())
    val target = Output(UInt(XLEN.W))
    val redirect = Output(Valid(new Redirect))
    val sourcePrivilege = Output(UInt(2.W))
    val sourceVirtual = Output(Bool())
    val accepts = Output(UInt(64.W))
    val allocations = Output(UInt(64.W))
    val robAllocation = Output(Vec(RenameWidth, Bool()))
    val robWritebackArrival = Output(Vec(rob.io.writeback.length, Valid(new RobPtr)))
    val csrSourcePc = Output(UInt(XLEN.W))
    val frontendCancel = Output(Bool())
    val dispatchPending = Output(Vec(RenameWidth, Valid(new FDICriticalSourceObservation)))
    val dispatchReady = Output(Vec(RenameWidth, Bool()))
    val jumpInputs = Output(UInt(64.W))
    val jumpOutputs = Output(UInt(64.W))
  })

  ctrl.rename.io.out.zipWithIndex.foreach { case (port, i) =>
    val payload = observe(port.bits)
    val dst = monitor.renamed(i)
    dst.valid := observe(port.valid) && observe(port.ready)
    dst.bits.pc := observe(ctrl.rename.io.in(i).bits.pc)
    dst.bits.instr := payload.instr
    dst.bits.robIdx := payload.robIdx
    dst.bits.ftqIdx := payload.ftqPtr
    dst.bits.ftqOffset := payload.ftqOffset
    dst.bits.pdest := payload.pdest
    dst.bits.operation := payload.fuOpType
    dst.bits.tag := payload.fdiNotTrusted.get
    dst.bits.waitForward := payload.waitForward
    dst.bits.blockBackward := payload.blockBackward
  }
  jumps.zip(monitor.jumps).foreach { case (unit, dst) =>
    val in = observe(unit.io.in.bits)
    val out = observe(unit.io.out.bits)
    dst.inValid := observe(unit.io.in.valid)
    dst.inReady := observe(unit.io.in.ready)
    dst.outValid := observe(unit.io.out.valid)
    dst.outReady := observe(unit.io.out.ready)
    dst.robIdx := in.ctrl.robIdx
    dst.ftqIdx := in.ctrl.ftqIdx.get
    dst.ftqOffset := in.ctrl.ftqOffset.get
    dst.basePc := in.data.pc.get
    dst.pdest := in.ctrl.pdest
    dst.operation := in.ctrl.fuOpType
    dst.tag := in.ctrl.fdiNotTrusted.get
    dst.result := out.res.data
    dst.rfWen := out.ctrl.rfWen.get
    dst.exception := out.ctrl.exceptionVec.get.asUInt
    dst.redirect := out.res.redirect.get
    dst.flush := observe(unit.io.flush)
  }
  ctrl.dispatch.io.fromRename.zipWithIndex.foreach { case (port, i) =>
    val payload = observe(port.bits)
    val dst = monitor.dispatchPending(i)
    dst.valid := observe(port.valid)
    dst.bits := 0.U.asTypeOf(dst.bits)
    dst.bits.instr := payload.instr
    dst.bits.robIdx := payload.robIdx
    dst.bits.ftqIdx := payload.ftqPtr
    dst.bits.ftqOffset := payload.ftqOffset
    monitor.dispatchReady(i) := observe(port.ready)
  }
  monitor.csrSourcePc := observe(csrMod.io.in.bits.sourcePc)
  monitor.frontendCancel := observe(ctrl.decode.io.redirect)
  monitor.robEmpty := observe(rob.isEmpty)
  monitor.waitForward := observe(rob.hasWaitForward)
  monitor.blockBackward := observe(rob.hasBlockBackward)
  // This observer tracks the real public reservation lifetime, not private state.
  val huOutstanding = RegInit(false.B)
  when(io.huReserve.valid && io.huReserveReady) { huOutstanding := true.B }
    .elsewhen(io.huRelease) { huOutstanding := false.B }
  monitor.huOutstanding := huOutstanding
  monitor.hasTrap := observe(csrMod.hasTrap)
  monitor.criticalDebug := observe(csrMod.debugMod.io.out.criticalErrorStateEnterDebug)
  monitor.targetUpdate := observe(csrMod.io.out.bits.targetPcUpdate)
  monitor.target := observe(csrMod.io.out.bits.targetPc.pc)
  monitor.redirect := observe(ctrl.io.redirect)
  monitor.sourcePrivilege := observe(backend.io.mem.tlbCsr.priv.imode)
  monitor.sourceVirtual := observe(backend.io.mem.csrCtrl.virtMode)

  def count(amount: UInt): UInt = {
    val total = RegInit(0.U(64.W))
    total := total + amount
    total
  }
  monitor.accepts := count(PopCount((io.instruction +: io.instructionTail.toSeq).map(_.fire)))
  // A dispatch request becomes a new ROB owner only at the native first-uop
  // allocation gate; a redirect can leave request bits visible without accepting it.
  monitor.robAllocation := VecInit(rob.io.enq.req.map { request =>
    observe(request.valid) && observe(rob.io.enq.canAccept) && observe(request.bits.firstUop) &&
      !observe(rob.io.redirect.valid)
  })
  monitor.allocations := count(PopCount(monitor.robAllocation))
  // This port carries arrivals registered after the previous cycle's filtering.
  // A new ROB redirect can still annul a same-cycle arrival; this is not a commit.
  // The separate exuWriteback port retains additional raw auxiliary bookkeeping.
  rob.io.writeback.zip(monitor.robWritebackArrival).foreach { case (source, destination) =>
    destination.valid := observe(source.valid)
    destination.bits := observe(source.bits.robIdx)
  }
  monitor.jumpInputs := count(PopCount(monitor.jumps.map(x => x.inValid && x.inReady)))
  monitor.jumpOutputs := count(PopCount(monitor.jumps.map(x => x.outValid && x.outReady)))

  // This repair owns only the cached scalar Store services. The production
  // parent remains independently buildable without later permission fixtures.
  val storeSystem = Module(new FDICriticalCachedStoreHarness)
  val pmp = withReset(reset.asAsyncReset) { Module(new PMP) }
  idleInputs(storeSystem.io)
  val memory = IO(new Bundle {
    val metadata = Vec(backendParams.StaCnt, new DCacheStoreIO)
    val cacheWrite = Flipped(new DCacheToSbufferIO)
    val ptw = new TlbPtwIOwithMemIdx(backendParams.StaCnt)
    val forceFlush = Input(Bool())
    val queueEmpty = Output(Bool())
    val bufferEmpty = Output(Bool())
    val flushDone = Output(Bool())
    val issue = Output(Vec(backendParams.StaCnt, Valid(new MemExuInput)))
    val dataIssue = Output(Vec(backendParams.StdCnt, Valid(new MemExuInput)))
    val completion = Output(Vec(backendParams.StaCnt, Valid(new MemExuOutput)))
    val phases = Output(Vec(backendParams.StaCnt, new FDICriticalStorePhaseObservation))
    val queueSlots = Output(chiselTypeOf(storeSystem.io.queueSlots))
    val publication = Output(chiselTypeOf(storeSystem.io.publication))
    val sourcePrivilege = Output(UInt(2.W))
    val sourceVirtual = Output(Bool())
    val queueCancel = Output(chiselTypeOf(storeSystem.io.queueCancel))
    val queueDequeue = Output(chiselTypeOf(storeSystem.io.queueDequeue))
    val cumulativeCompletions = Output(UInt(64.W))
    val cumulativePublications = Output(UInt(64.W))
  })
  val memoryContext = DelayN(backend.io.mem.tlbCsr, 2)
  val memoryControls = DelayN(backend.io.mem.csrCtrl, 2)
  storeSystem.io.tlbCsr := memoryContext
  storeSystem.io.csrCtrl := memoryControls
  storeSystem.io.sfence := DelayN(backend.io.mem.sfence, 2)
  storeSystem.io.redirect := RegNextWithEnable(backend.io.mem.redirect)
  pmp.io.distribute_csr := memoryControls.distribute_csr
  storeSystem.io.pmpEnvironment.pmp := pmp.io.pmp
  storeSystem.io.pmpEnvironment.pma := pmp.io.pma
  storeSystem.io.pmpEnvironment.mode := memoryContext.priv.dmode
  storeSystem.io.pmpEnvironment.cmode := (if (HasBitmapCheck) memoryContext.mbmc.CMODE.asBool else true.B)
  memory.sourcePrivilege := memoryContext.priv.imode
  memory.sourceVirtual := memoryControls.virtMode
  memory.metadata <> storeSystem.io.metadata
  memory.cacheWrite <> storeSystem.io.cacheWrite
  memory.ptw <> storeSystem.io.ptw
  storeSystem.io.flushBuffer := memory.forceFlush || RegNext(backend.io.fenceio.sbuffer.flushSb, false.B)
  backend.io.fenceio.sbuffer.sbIsEmpty := RegNext(storeSystem.io.flushDone, false.B)
  storeSystem.io.rob <> backend.io.mem.robLsqIO
  storeSystem.io.enq.lqCanAccept := true.B
  backend.io.mem.sqCanAccept := storeSystem.io.enq.canAccept
  backend.io.mem.lsqEnqIO.canAccept := storeSystem.io.enq.canAccept
  backend.io.mem.sqDeq := storeSystem.io.queueDequeue
  backend.io.mem.sqCancelCnt := storeSystem.io.queueCancel
  backend.io.mem.sqDeqPtr := observe(storeSystem.queue.io.sqDeqPtr)
  backend.io.mem.stIssuePtr := observe(storeSystem.queue.io.stAddrReadySqPtr)
  backend.io.mem.exceptionAddr.vaddr := observe(storeSystem.queue.io.exceptionAddr.vaddr)
  backend.io.mem.exceptionAddr.gpaddr := observe(storeSystem.queue.io.exceptionAddr.gpaddr)
  backend.io.mem.exceptionAddr.isForVSnonLeafPTE := observe(storeSystem.queue.io.exceptionAddr.isForVSnonLeafPTE)
  storeSystem.io.enq.req.indices.foreach { lane =>
    val request = backend.io.mem.lsqEnqIO.req(lane)
    val needsStore = backend.io.mem.lsqEnqIO.needAlloc(lane)(1)
    storeSystem.io.enq.needAlloc(lane) := needsStore
    storeSystem.io.enq.req(lane).valid := request.valid && needsStore
    storeSystem.io.enq.req(lane).bits := request.bits
    backend.io.mem.lsqEnqIO.resp(lane).sqIdx := storeSystem.io.enq.resp(lane)
    backend.io.mem.lsqEnqIO.resp(lane).lqIdx := 0.U.asTypeOf(new LqPtr)
  }
  for (lane <- 0 until backendParams.StaCnt) {
    storeSystem.io.sta(lane) <> backend.io.mem.issueSta(lane)
    backend.io.mem.staIqFeedback(lane).feedbackSlow := storeSystem.io.feedback(lane)
    backend.io.mem.stIn(lane).valid := observe(storeSystem.stores(lane).io.issue.valid)
    backend.io.mem.stIn(lane).bits := observe(storeSystem.stores(lane).io.issue.bits.uop)
    backend.io.mem.writebackSta(lane).valid := storeSystem.io.wb(lane).valid
    backend.io.mem.writebackSta(lane).bits := storeSystem.io.wb(lane).bits
    when(storeSystem.io.wb(lane).valid) {
      assert(backend.io.mem.writebackSta(lane).ready, "The native scalar Store completion is always consumed")
    }
    memory.issue(lane).valid := storeSystem.io.sta(lane).fire
    memory.issue(lane).bits := storeSystem.io.sta(lane).bits
    memory.completion(lane) := storeSystem.io.wb(lane)
  }
  for (lane <- 0 until backendParams.StdCnt) {
    storeSystem.io.std(lane) <> backend.io.mem.issueStd(lane)
    backend.io.mem.writebackStd(lane).valid := storeSystem.io.dataWb(lane).valid
    backend.io.mem.writebackStd(lane).bits := storeSystem.io.dataWb(lane).bits
    when(storeSystem.io.dataWb(lane).valid) {
      assert(backend.io.mem.writebackStd(lane).ready, "The native scalar Store data completion is always consumed")
    }
    memory.dataIssue(lane).valid := storeSystem.io.std(lane).fire
    memory.dataIssue(lane).bits := storeSystem.io.std(lane).bits
  }
  memory.cumulativeCompletions := count(PopCount(memory.completion.map(_.valid)))
  memory.cumulativePublications := count(PopCount(storeSystem.io.publication.map(_.fire)))
  memory.phases := storeSystem.io.phases
  memory.queueSlots := storeSystem.io.queueSlots
  memory.publication := storeSystem.io.publication
  memory.queueEmpty := storeSystem.io.queueEmpty
  memory.bufferEmpty := storeSystem.io.bufferEmpty
  memory.flushDone := storeSystem.io.flushDone
  memory.queueCancel := storeSystem.io.queueCancel
  memory.queueDequeue := storeSystem.io.queueDequeue
}

// All address, data, retirement and publication decisions stay in native modules.
// The exposed PTW and cache ports are the services normally supplied by MemBlock.
class FDICriticalCachedStoreHarness(implicit p: Parameters) extends XSModule {
  require(backendParams.StaCnt == 2 && backendParams.StdCnt == 2 && backendParams.HyuCnt == 0)
  require(EnsbufferWidth == 2 && HasVPU && VLEN == 128)
  val stores = Seq.fill(backendParams.StaCnt)(withReset(reset.asAsyncReset) { Module(new StoreUnit) })
  private val stdParameters = backendParams.memSchdParams.get.issueBlockParams
    .find(_.StdCnt != 0).get.exuBlockParams.head
  val dataUnits = Seq.fill(backendParams.StdCnt)(withReset(reset.asAsyncReset) {
    Module(new MemExeUnit(stdParameters))
  })
  val translation = withReset(reset.asAsyncReset) { Module(new TLBNonBlock(backendParams.StaCnt, 1, sttlbParams)) }
  val protection = Seq.fill(backendParams.StaCnt)(withReset(reset.asAsyncReset) { Module(new PMPChecker(4, leaveHitMux = true)) })
  val queue = withReset(reset.asAsyncReset) { Module(new StoreQueue) }
  val buffer = withReset(reset.asAsyncReset) { Module(new Sbuffer) }
  val io = IO(new Bundle {
    val enq = new SqEnqIO
    val sta = Vec(stores.size, Flipped(Decoupled(new MemExuInput)))
    val std = Vec(dataUnits.size, Flipped(Decoupled(new MemExuInput)))
    val wb = Output(Vec(stores.size, Valid(new MemExuOutput)))
    val dataWb = Output(Vec(dataUnits.size, Valid(new MemExuOutput)))
    val feedback = Output(Vec(stores.size, Valid(new RSFeedback)))
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val tlbCsr = Input(new TlbCsrBundle)
    val sfence = Input(new SfenceBundle)
    val redirect = Flipped(Valid(new Redirect))
    val rob = Flipped(new RobLsqIO)
    val pmpEnvironment = Input(new PMPCheckerEnv)
    val ptw = new TlbPtwIOwithMemIdx(stores.size)
    val metadata = Vec(stores.size, new DCacheStoreIO)
    val cacheWrite = Flipped(new DCacheToSbufferIO)
    val flushBuffer = Input(Bool())
    val bufferEmpty = Output(Bool())
    val flushDone = Output(Bool())
    val queueEmpty = Output(Bool())
    val queueDequeue = Output(chiselTypeOf(queue.io.sqDeq))
    val queueCancel = Output(chiselTypeOf(queue.io.sqCancelCnt))
    val phases = Output(Vec(stores.size, new FDICriticalStorePhaseObservation))
    val queueSlots = Output(Vec(StoreQueueSize, new Bundle {
      val allocated = Bool()
      val committed = Bool()
      val cancel = Bool()
      val rob = new RobPtr
    }))
    val publication = Output(Vec(EnsbufferWidth, new Bundle {
      val valid = Bool()
      val ready = Bool()
      val fire = Bool()
      val bits = new DCacheWordReqWithVaddrAndPfFlag
      val sq = new SqPtr
    }))
  })
  private def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  (stores.map(_.io) ++ dataUnits.map(_.io) ++ protection.map(_.io) ++ Seq(translation.io, queue.io, buffer.io))
    .foreach(idleInputs)
  queue.io.enq <> io.enq
  queue.io.brqRedirect := io.redirect
  queue.io.rob <> io.rob
  // No Load engine is present. StoreQueue leaves these unrelated outputs unused;
  // drive the public ROB cutpoint explicitly so it cannot classify a head as MMIO.
  io.rob.mmio := VecInit(Seq.fill(LoadPipelineWidth)(false.B))
  io.rob.uop := 0.U.asTypeOf(io.rob.uop)
  queue.io.hartId := 0.U
  queue.io.exceptionAddr.isStore := true.B
  queue.io.mmioStout.ready := true.B
  queue.io.cboZeroStout.ready := true.B
  queue.io.vecmmioStout.ready := true.B
  assert(!queue.io.uncache.req.valid && !queue.io.mmioStout.valid && !queue.io.cboZeroStout.valid,
    "Only aligned ordinary cacheable Stores belong to this fixture")
  io.queueEmpty := queue.io.sqEmpty
  io.queueDequeue := queue.io.sqDeq
  io.queueCancel := queue.io.sqCancelCnt
  translation.io.csr := io.tlbCsr
  translation.io.sfence := io.sfence
  translation.io.redirect := io.redirect
  translation.io.requestor.foreach(_.resp.ready := true.B)
  io.ptw <> translation.io.ptw
  if (sttlbParams.outReplace) {
    val replacement = Module(new TlbReplace(stores.size, sttlbParams))
    replacement.io.apply_sep(Seq(translation.io.replace), io.ptw.resp.bits.s1.entry.tag)
  }
  buffer.io.hartId := 0.U
  buffer.io.csrCtrl := io.csrCtrl
  buffer.io.sqempty := queue.io.sqEmpty
  buffer.io.force_write := queue.io.force_write
  buffer.io.in <> queue.io.sbuffer
  buffer.io.vecDifftestInfo <> queue.io.sbufferVecDifftestInfo
  buffer.io.flush.valid := io.flushBuffer || queue.io.flushSbuffer.valid
  queue.io.flushSbuffer.empty := buffer.io.flush.empty
  io.bufferEmpty := buffer.io.sbempty
  io.flushDone := buffer.io.flush.empty
  io.cacheWrite <> buffer.io.dcache
  stores.zipWithIndex.foreach { case (store, lane) =>
    store.io.stin <> io.sta(lane)
    store.io.redirect := io.redirect
    store.io.csrCtrl := io.csrCtrl
    store.io.tlb <> translation.io.requestor(lane)
    protection(lane).io.check_env := io.pmpEnvironment
    protection(lane).io.req := translation.io.pmp(lane)
    store.io.pmp := protection(lane).io.resp
    io.metadata(lane) <> store.io.dcache
    store.io.stout.ready := true.B
    store.io.vecstout.ready := true.B
    store.io.prefetch_req <> buffer.io.store_prefetch(lane)
    queue.io.storeAddrIn(lane) := store.io.lsq
    queue.io.storeAddrInRe(lane) := store.io.lsq_replenish
    queue.io.storeMaskIn(lane) := store.io.st_mask_out
    assert(!store.io.misalign_enq.req.valid, "No split Store is part of the critical recovery program")
    io.wb(lane).valid := store.io.stout.fire
    io.wb(lane).bits := store.io.stout.bits
    io.feedback(lane) := store.io.feedback_slow
    val phase = io.phases(lane)
    phase.redirect := store.io.redirect
    phase.s0Fire := observe(store.s0_fire)
    phase.s0Kill := observe(store.s0_kill)
    phase.s1Valid := observe(store.s1_valid)
    phase.s1Fire := observe(store.s1_fire)
    phase.s1Kill := observe(store.s1_kill)
    phase.s1Rob := observe(store.s1_in.uop.robIdx)
    phase.s1Sq := observe(store.s1_in.uop.sqIdx)
    phase.s2Valid := observe(store.s2_valid)
    phase.s2Fire := observe(store.s2_fire)
    phase.s2Kill := observe(store.s2_kill)
    phase.s2Rob := observe(store.s2_in.uop.robIdx)
    phase.s2Sq := observe(store.s2_in.uop.sqIdx)
    phase.translation.valid := store.io.tlb.req.fire
    phase.translation.bits := store.io.tlb.req.bits
  }
  dataUnits.zipWithIndex.foreach { case (unit, lane) =>
    unit.io.in <> io.std(lane)
    unit.io.flush := io.redirect
    unit.io.out.ready := true.B
    queue.io.storeDataIn(lane).valid := unit.io.out.valid
    queue.io.storeDataIn(lane).bits := 0.U.asTypeOf(queue.io.storeDataIn(lane).bits)
    queue.io.storeDataIn(lane).bits.uop := unit.io.out.bits.uop
    queue.io.storeDataIn(lane).bits.data := unit.io.out.bits.data
    io.dataWb(lane).valid := unit.io.out.fire
    io.dataWb(lane).bits := unit.io.out.bits
  }
  for (index <- 0 until StoreQueueSize) {
    io.queueSlots(index).allocated := observe(queue.allocated(index))
    io.queueSlots(index).committed := observe(queue.committed(index))
    io.queueSlots(index).cancel := observe(queue.needCancel(index))
    io.queueSlots(index).rob := observe(queue.uop(index).robIdx)
  }
  for (lane <- 0 until EnsbufferWidth) {
    io.publication(lane).valid := buffer.io.in(lane).valid
    io.publication(lane).ready := buffer.io.in(lane).ready
    io.publication(lane).fire := buffer.io.in(lane).fire
    io.publication(lane).bits := buffer.io.in(lane).bits
    io.publication(lane).sq := observe(queue.dataBuffer.io.deq(lane).bits.sqPtr)
  }
}

object FDICriticalRecoveryOffMain extends App {
  require(!sys.env("CRITICAL_FDI_ENABLED").toBoolean)
  implicit val p: Parameters = UserTimerDeliveryParameters(enabled = false)
  val debug = p(DebugOptionsKey)
  utility.Constantin.init(debug.EnableConstantin && !debug.FPGAPlatform)
  utility.ChiselDB.init(debug.EnableChiselDB && !debug.FPGAPlatform)
  val root = java.nio.file.Paths.get(sys.env("CRITICAL_RUN_ROOT")).toRealPath()
  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(new UserTimerDeliveryHarness,
    Array("--target-dir", root.resolve("rtl").toString),
    Array("--disable-all-randomization", "--strip-debug-info"))
}
