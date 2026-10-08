// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import utility.DelayN
import xiangshan._
import xiangshan.backend.BackendInlinedImp
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.datapath.{BypassNetwork, WbDataPath}
import xiangshan.backend.issue.FpScheduler
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.{DCacheWordReq, DCacheWordResp}
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.Bundles._

class FDILoadConsumerPhase(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val ready = Bool()
  val rob = new RobPtr
  val destination = UInt(PhyRegIdxWidth.W)
  val floating = Bool()
  val dependency = Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))
  val operand = UInt(XLEN.W)
  val entryClass = UInt(2.W)
  val queue = UInt(8.W)
  // The directed scalar consumers use at most three architectural sources.
  val sourceRegister = Vec(3, UInt(PhyRegIdxWidth.W))
  val sourceFloating = Vec(3, Bool())
  val sourceUsed = Vec(3, Bool())
  val sourceReady = Vec(3, Bool())
  val registerRead = Vec(3, Bool())
}

// Real load and queue instances replace the inherited idle memory inputs.
// Every observation below reads production state; no tap drives it back.
class FDILoadPermissionBackendHarness(implicit p: Parameters) extends UserTimerDeliveryHarness {
  val loads = Seq.fill(backendParams.LduCnt)(withReset(reset.asAsyncReset) { Module(new LoadUnit) })
  val queue = withReset(reset.asAsyncReset) { Module(new LoadQueue) }
  private val tlbWidth = backendParams.LduCnt + backendParams.HyuCnt + 1
  val tlb = withReset(reset.asAsyncReset) { Module(new TLBNonBlock(tlbWidth, 2, ldtlbParams)) }
  require(backendParams.HyuCnt == 0 && !EnableLoadToLoadForward)
  val units = Seq(backendOuter.inner.intExuBlock, backendOuter.inner.fpExuBlock)
    .flatten.flatMap(_.exus).flatMap(_.module.funcUnits)
  private val backendInner = backendOuter.inner.module
  private val actualDataPath = backendOuter.inner.dataPath.module
  private val queues = Seq(backendOuter.inner.intScheduler, backendOuter.inner.fpScheduler,
    backendOuter.inner.memScheduler).flatten.flatMap(_.module.issueQueues)
  private val queueEntries = queues.flatMap(queue => queue.entries.entries.indices.map(index => (queue, index)))
  private val queueInputs = queues.flatMap(_.io.enq)
  private val queueDequeues = queues.flatMap(_.deqBeforeDly)
  private val queueDelayed = queues.flatMap(_.io.deqDelay)
  private val queueWakeups = queues.flatMap(_.io.wakeupToIQ)
  private val timingQueueEntries = queueEntries.filter(_._1.params.schdType != FpScheduler())
  private val incomingWakeups = queues.zipWithIndex.filter(_._1.params.schdType != FpScheduler())
    .flatMap { case (queue, index) => queue.io.wakeupFromIQ.map(source => (index, source)) }
  private val queueInputOwners = queues.zipWithIndex.flatMap { case (queue, index) => Seq.fill(queue.io.enq.size)(index) }
  private val queueDequeueOwners = queues.zipWithIndex.flatMap { case (queue, index) => Seq.fill(queue.io.deqDelay.size)(index) }
  private val fpWbWakeups = queues.zipWithIndex.filter(_._1.params.schdType == FpScheduler())
    .flatMap { case (queue, index) => queue.io.wakeupFromWB.map(index -> _) }
  private val dataInputs = (actualDataPath.io.fromIntIQ ++ actualDataPath.io.fromFpIQ ++ actualDataPath.io.fromMemIQ).flatten.toSeq
  private val dataOutputs = (actualDataPath.io.toIntExu ++ actualDataPath.io.toFpExu ++ actualDataPath.io.toMemExu).flatten.toSeq
  require(dataInputs.size == queueDequeueOwners.size)

  // These two existing non-diplomatic children have no public accessor. The
  // approved test-only lookup is exact, typed and read-only; absence is fatal.
  private val bypassFields = classOf[BackendInlinedImp].getDeclaredFields.filter(field =>
    Set("bypassNetwork", "xiangshan$backend$BackendInlinedImp$$bypassNetwork").contains(field.getName))
  require(bypassFields.length == 1 && classOf[BypassNetwork].isAssignableFrom(bypassFields.head.getType))
  bypassFields.head.setAccessible(true)
  val actualBypass = bypassFields.head.get(backendInner).asInstanceOf[BypassNetwork]
  require(actualBypass eq bypassFields.head.get(backendInner))
  private val wbFields = classOf[BackendInlinedImp].getDeclaredFields.filter(field =>
    Set("wbDataPath", "xiangshan$backend$BackendInlinedImp$$wbDataPath").contains(field.getName))
  require(wbFields.length == 1 && classOf[WbDataPath].isAssignableFrom(wbFields.head.getType))
  wbFields.head.setAccessible(true)
  val actualWriteback = wbFields.head.get(backendInner).asInstanceOf[WbDataPath]
  require(actualWriteback eq wbFields.head.get(backendInner))
  private val rcSources = (actualBypass.io.fromExus.int ++ actualBypass.io.fromExus.fp ++
    actualBypass.io.fromExus.vf ++ actualBypass.io.fromExus.mem).flatten.filter(_.bits.params.needWriteRegCache)
  require(rcSources.size == actualBypass.io.toDataPath.size)

  val memory = IO(new Bundle {
    val executionRedirect = Output(Valid(new Redirect))
    val cacheReady = Input(Vec(loads.size, Bool()))
    val response = Input(Vec(loads.size, Valid(new DCacheWordResp)))
    val pmp = Input(Vec(loads.size, new PMPRespBundle))
    val requests = Output(Vec(loads.size, Valid(new DCacheWordReq)))
    val issue = Output(Vec(loads.size, Valid(new MemExuInput)))
    val completion = Output(Vec(loads.size, Valid(new MemExuOutput)))
    val noData = Output(Vec(loads.size, Bool()))
    val ld1Cancel = Output(Vec(loads.size, Bool()))
    val ld2Cancel = Output(Vec(loads.size, Bool()))
    val wakeup = Output(Vec(loads.size, Bool()))
    val iqEntries = Output(Vec(queueEntries.size, new FDILoadConsumerPhase))
    val iqEnqueue = Output(Vec(queueInputs.size, new FDILoadConsumerPhase))
    val iqDequeue = Output(Vec(queueDequeues.size, new FDILoadConsumerPhase))
    val iqDelayed = Output(Vec(queueDelayed.size, new FDILoadConsumerPhase))
    val iqWakeup = Output(Vec(queueWakeups.size, new Bundle {
      val valid = Bool()
      val destination = UInt(PhyRegIdxWidth.W)
      val integer = Bool()
      val floating = Bool()
      val dependency = Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))
    }))
    // Entry status is observed before the outgoing dependency shift. An
    // unready stored dependency is not an accepted downstream transaction.
    val iqState = Output(Vec(timingQueueEntries.size, new Bundle {
      val valid = Bool()
      val rob = new RobPtr
      val destination = UInt(PhyRegIdxWidth.W)
      val queue = UInt(8.W)
      val initial = Bool()
      val sourceRegister = Vec(3, UInt(PhyRegIdxWidth.W))
      val sourceUsed = Vec(3, Bool())
      val sourceReady = Vec(3, Bool())
      val sourceDependency = Vec(3, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
      val storedCancel = Vec(3, Bool())
      val transferCancel = Vec(3, Bool())
    }))
    val incomingWakeup = Output(Vec(incomingWakeups.size, new Bundle {
      val valid = Bool()
      val integer = Bool()
      val destination = UInt(PhyRegIdxWidth.W)
      val queue = UInt(8.W)
      val sourceLoadLane = SInt(8.W)
      val dependency = Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))
    }))
    val fpWbWakeup = Output(Vec(fpWbWakeups.size, new Bundle {
      val valid = Bool()
      val floating = Bool()
      val destination = UInt(PhyRegIdxWidth.W)
      val queue = UInt(8.W)
    }))
    val dataInput = Output(Vec(dataInputs.size, new FDILoadConsumerPhase))
    val dataOutput = Output(Vec(dataOutputs.size, new FDILoadConsumerPhase))
    val registerWrites = Output(Vec(actualWriteback.io.toIntPreg.size + actualWriteback.io.toFpPreg.size, new Bundle {
      val valid = Bool()
      val floating = Bool()
      val destination = UInt(PhyRegIdxWidth.W)
      val data = UInt(XLEN.W)
    }))
    val rcWrites = Output(Vec(actualBypass.io.toDataPath.size, new Bundle {
      val valid = Bool()
      val data = UInt(XLEN.W)
      val sourceValid = Bool()
      val sourceDestination = UInt(PhyRegIdxWidth.W)
    }))
    val ptw = new TlbPtwIOwithMemIdx(tlbWidth)
    val queueEmpty = Output(Bool())
    val queueCancel = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
    val uncacheRequest = Output(Bool())
    val splitRequest = Output(Vec(loads.size, Bool()))
    val mirror = Output(Vec(34, UInt(64.W)))
    val reason = Output(UInt(64.W))
    val reasonEffect = Output(Bool())
    val supervisorTval = Output(UInt(64.W))
    val sourcePrivilege = Output(UInt(2.W))
    val sourceVirtual = Output(Bool())
    val dataPrivilege = Output(UInt(2.W))
    val dataVirtual = Output(Bool())
    val cacheErrorsEnabled = Output(Bool())
    val distribution = Output(new DistributedCSRIO)
    val permissionRequest = Output(Vec(loads.size, Valid(new FDIPermissionRequest)))
    val permissionResponse = Output(Vec(loads.size, Valid(new FDIPermissionResponse)))
    val rename = Output(Vec(RenameWidth, Valid(new DynInst)))
    val execution = Output(Vec(units.size, Valid(new Bundle {
      val rob = new RobPtr
      val destination = UInt(PhyRegIdxWidth.W)
      val operand = UInt(XLEN.W)
      val floating = Bool()
    })))
    val bypass = Output(Vec(actualBypass.io.fromExus.mem.flatten.size, new Bundle {
      val valid = Bool()
      val data = UInt(128.W)
      val intWen = Bool()
      val destination = UInt(PhyRegIdxWidth.W)
    }))
    val wb = Output(Vec(actualWriteback.io.fromMemExu.flatten.size, new Bundle {
      val fire = Bool()
      val rob = new RobPtr
      val intWen = Bool()
      val fpWen = Bool()
      val exceptions = ExceptionVec()
    }))
  })

  loads.foreach(load => idleInputs(load.io))
  idleInputs(queue.io)
  idleInputs(tlb.io)
  // ROB, dispatch and Mem consume this original redirect. The later frontend
  // export can normalize its level and is only a recovery-target observation.
  memory.executionRedirect := backend.io.mem.redirect
  val context = DelayN(backend.io.mem.tlbCsr, 2)
  val controls = DelayN(backend.io.mem.csrCtrl, 2)
  memory.sourcePrivilege := context.priv.imode
  memory.sourceVirtual := controls.virtMode
  memory.dataPrivilege := context.priv.dmode
  memory.dataVirtual := context.priv.virt
  memory.cacheErrorsEnabled := controls.cache_error_enable
  memory.distribution := backend.io.mem.csrCtrl.distribute_csr
  // Match MemBlock's existing trigger transport after its CSR control delay.
  // All configuration originates in actual CSR instructions, not test actions.
  private val triggerData = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  private val triggerEnable = RegInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  triggerEnable := controls.mem_trigger.tEnableVec
  when(controls.mem_trigger.tUpdate.valid) {
    triggerData(controls.mem_trigger.tUpdate.bits.addr) := controls.mem_trigger.tUpdate.bits.tdata
  }
  tlb.io.csr := context
  tlb.io.sfence := DelayN(backend.io.mem.sfence, 2)
  tlb.io.redirect := backend.io.mem.redirect
  tlb.io.requestor.foreach(_.resp.ready := true.B)
  memory.ptw <> tlb.io.ptw
  if (ldtlbParams.outReplace) {
    val replacement = Module(new TlbReplace(tlbWidth, ldtlbParams))
    replacement.io.apply_sep(Seq(tlb.io.replace), memory.ptw.resp.bits.s1.entry.tag)
  }
  val mirror = Option.when(HasFDI) {
    val instance = withReset(reset.asBool) { Module(new FDICSRMirror(FDIMirrorClient.Memory)) }
    instance.io.distribute := controls.distribute_csr
    instance.io.distribute.w.valid := RegNext(RegNext(backend.io.mem.csrCtrl.distribute_csr.w.valid, false.B), false.B)
    instance
  }
  memory.mirror := mirror.map(_.io.state).getOrElse(0.U.asTypeOf(memory.mirror))
  memory.reason := (if (HasFDI) observe(csrMod.csrOutMap(0x8b3)) else 0.U)
  memory.reasonEffect := (if (HasFDI) {
    val owner = csrMod.csrMods.find(_.addr == 0x8b3).get.asInstanceOf[FDIFReasonModule]
    observe(owner.trapReason.valid)
  } else false.B)
  memory.supervisorTval := observe(csrMod.stval.rdata)
  val config = Option.when(HasFDI) {
    val value = Wire(new FDIMemoryConfig)
    val main = Wire(new FDIMainCfgBundle)
    val library = Wire(new FDILibCfgBundle)
    main := mirror.get.word(FDIMainCfgAddress.sMainCfg)
    library := mirror.get.word(FDIBoundRegisterAddress.libCfg)
    value.policy.uEnable := main.uEnable.asBool
    value.policy.sEnable := main.sEnable.asBool
    value.policy.uCloseRead := main.uCloseRead.asBool
    value.policy.uCloseWrite := main.uCloseWrite.asBool
    value.policy.uCloseJump := main.uCloseJump.asBool
    value.policy.uCloseEcall := main.uCloseEcall.asBool
    value.policy.sCloseRead := main.sCloseRead.asBool
    value.policy.sCloseWrite := main.sCloseWrite.asBool
    value.policy.sCloseJump := main.sCloseJump.asBool
    value.policy.sCloseEcall := main.sCloseEcall.asBool
    value.sourcePrivilege := context.priv.imode
    value.sourceVirtual := controls.virtMode
    value.entries.zipWithIndex.foreach { case (entry, index) =>
      entry.boundLo := mirror.get.word(FDIBoundRegisterAddress.libBoundLo0 + 2 * index)
      entry.boundHi := mirror.get.word(FDIBoundRegisterAddress.libBoundHi0 + 2 * index)
      entry.entryValid := library.elements(s"V$index").asUInt.asBool
      entry.readAllowed := library.elements(s"R$index").asUInt.asBool
      entry.writeAllowed := library.elements(s"W$index").asUInt.asBool
    }
    value
  }

  queue.io.redirect := backend.io.mem.redirect
  queue.io.rob <> backend.io.mem.robLsqIO
  queue.io.enq.sqCanAccept := true.B
  backend.io.mem.lqCanAccept := queue.io.enq.canAccept
  backend.io.mem.lsqEnqIO.canAccept := queue.io.enq.canAccept
  backend.io.mem.lqDeq := queue.io.lqDeq
  backend.io.mem.lqCancelCnt := queue.io.lqCancelCnt
  backend.io.mem.lqDeqPtr := queue.io.lqDeqPtr
  queue.io.enq.req.indices.foreach { index =>
    val source = backend.io.mem.lsqEnqIO.req(index)
    queue.io.enq.needAlloc(index) := backend.io.mem.lsqEnqIO.needAlloc(index)(0)
    queue.io.enq.req(index).valid := source.valid && backend.io.mem.lsqEnqIO.needAlloc(index)(0)
    queue.io.enq.req(index).bits := source.bits
    queue.io.enq.req(index).bits.sqIdx := 0.U.asTypeOf(new SqPtr)
    backend.io.mem.lsqEnqIO.resp(index).lqIdx := queue.io.enq.resp(index)
    backend.io.mem.lsqEnqIO.resp(index).sqIdx := 0.U.asTypeOf(new SqPtr)
  }
  queue.io.sq.sqEmpty := true.B
  queue.io.sq.stAddrReadyVec.foreach(_ := true.B)
  queue.io.sq.stDataReadyVec.foreach(_ := true.B)
  queue.io.exceptionAddr.isStore := false.B
  backend.io.mem.exceptionAddr.vaddr := queue.io.exceptionAddr.vaddr
  backend.io.mem.exceptionAddr.gpaddr := queue.io.exceptionAddr.gpaddr
  backend.io.mem.exceptionAddr.isForVSnonLeafPTE := queue.io.exceptionAddr.isForVSnonLeafPTE
  queue.io.uncache.req.ready := true.B
  queue.io.noUopsIssed := !VecInit(backend.io.mem.issueLda.map(_.valid)).asUInt.orR
  queue.io.tlbReplayDelayCycleCtrl.foreach(_ := 0.U)
  memory.queueEmpty := queue.io.lqEmpty
  memory.queueCancel := queue.io.lqCancelCnt
  memory.uncacheRequest := queue.io.uncache.req.valid

  loads.zipWithIndex.foreach { case (load, lane) =>
    load.io.ldin <> backend.io.mem.issueLda(lane)
    backend.io.mem.writebackLda(lane) <> load.io.ldout
    backend.io.mem.ldaIqFeedback(lane).feedbackSlow := load.io.feedback_slow
    backend.io.mem.wakeup(lane) := load.io.wakeup
    backend.io.mem.otherFastWakeup(lane) := load.io.fast_uop
    backend.io.mem.ldCancel(lane) := load.io.ldCancel
    backend.io.mem.s3_delayed_load_error(lane) := load.io.s3_dly_ld_err
    load.io.redirect := backend.io.mem.redirect
    load.io.csrCtrl := controls
    load.io.fromCsrTrigger.tdataVec := triggerData
    load.io.fromCsrTrigger.tEnableVec := triggerEnable
    load.io.fromCsrTrigger.triggerCanRaiseBpExp := controls.mem_trigger.triggerCanRaiseBpExp
    load.io.fromCsrTrigger.debugMode := controls.mem_trigger.debugMode
    load.io.fdiConfig.foreach(_ := config.get)
    load.io.tlb <> tlb.io.requestor(lane)
    load.io.pmp := memory.pmp(lane)
    load.io.dcache.req.ready := memory.cacheReady(lane)
    load.io.dcache.resp.valid := memory.response(lane).valid
    load.io.dcache.resp.bits := memory.response(lane).bits
    load.io.dcache.s2_hit := !memory.response(lane).bits.miss
    load.io.dcache.s2_first_hit := !memory.response(lane).bits.miss
    load.io.fast_rep_in <> load.io.fast_rep_out
    load.io.replay <> queue.io.replay(lane)
    load.io.lsq.ldin <> queue.io.ldu.ldin(lane)
    load.io.lsq.ldld_nuke_query <> queue.io.ldu.ldld_nuke_query(lane)
    load.io.lsq.stld_nuke_query <> queue.io.ldu.stld_nuke_query(lane)
    load.io.lsq.uncache <> queue.io.ldout(lane)
    load.io.lsq.nc_ldin <> queue.io.ncOut(lane)
    load.io.lsq.ld_raw_data := queue.io.ld_raw_data(lane)
    load.io.lsq.lqDeqPtr := queue.io.lqDeqPtr
    load.io.lq_rep_full := queue.io.lq_rep_full
    memory.requests(lane).valid := load.io.dcache.req.fire
    memory.requests(lane).bits := load.io.dcache.req.bits
    memory.issue(lane).valid := load.io.ldin.fire
    memory.issue(lane).bits := load.io.ldin.bits
    memory.completion(lane).valid := load.io.ldout.fire
    memory.completion(lane).bits := load.io.ldout.bits
    memory.noData(lane) := (if (HasFDI) observe(load.s2_valid) && observe(load.s2_fdiBlocked) else false.B)
    memory.ld1Cancel(lane) := load.io.ldCancel.ld1Cancel
    memory.ld2Cancel(lane) := load.io.ldCancel.ld2Cancel
    memory.wakeup(lane) := load.io.wakeup.valid
    memory.splitRequest(lane) := load.io.misalign_enq.req.valid
    memory.permissionRequest(lane) := 0.U.asTypeOf(memory.permissionRequest(lane))
    memory.permissionResponse(lane) := 0.U.asTypeOf(memory.permissionResponse(lane))
    load.fdiPermission.foreach { permission =>
      memory.permissionRequest(lane).valid := observe(permission.io.req.valid) && observe(permission.io.req.ready)
      memory.permissionRequest(lane).bits := observe(permission.io.req.bits)
      memory.permissionResponse(lane).valid := observe(permission.io.resp.valid) && observe(permission.io.resp.ready)
      memory.permissionResponse(lane).bits := observe(permission.io.resp.bits)
    }
  }
  ctrl.rename.io.out.zipWithIndex.foreach { case (port, index) =>
    memory.rename(index).valid := observe(port.valid) && observe(port.ready)
    memory.rename(index).bits := observe(port.bits)
  }
  units.zipWithIndex.foreach { case (unit, index) =>
    memory.execution(index).valid := observe(unit.io.in.valid) && observe(unit.io.in.ready)
    memory.execution(index).bits.rob := observe(unit.io.in.bits.ctrl.robIdx)
    memory.execution(index).bits.destination := observe(unit.io.in.bits.ctrl.pdest)
    memory.execution(index).bits.operand := unit.io.in.bits.data.src.headOption.map(observe(_)).getOrElse(0.U)
    memory.execution(index).bits.floating := unit.io.in.bits.ctrl.fpWen.map(observe(_)).getOrElse(false.B)
  }
  actualBypass.io.fromExus.mem.flatten.zipWithIndex.foreach { case (port, index) =>
    memory.bypass(index).valid := observe(port.valid)
    memory.bypass(index).data := observe(port.bits.data)
    memory.bypass(index).intWen := observe(port.bits.intWen)
    memory.bypass(index).destination := observe(port.bits.pdest)
  }
  actualWriteback.io.fromMemExu.flatten.zipWithIndex.foreach { case (port, index) =>
    memory.wb(index).fire := observe(port.valid) && observe(port.ready)
    memory.wb(index).rob := observe(port.bits.robIdx)
    memory.wb(index).intWen := port.bits.intWen.map(observe(_)).getOrElse(false.B)
    memory.wb(index).fpWen := port.bits.fpWen.map(observe(_)).getOrElse(false.B)
    memory.wb(index).exceptions := port.bits.exceptionVec.map(observe(_)).getOrElse(0.U.asTypeOf(ExceptionVec()))
  }
  queueEntries.zip(memory.iqEntries).foreach { case ((queue, index), sink) =>
    val entry = observe(queue.entries.entries(index))
    sink := 0.U.asTypeOf(sink)
    sink.valid := entry.valid
    sink.rob := entry.bits.status.robIdx
    sink.destination := entry.bits.payload.pdest
    sink.floating := entry.bits.payload.fpWen
    sink.dependency := observe(queue.entries.io.loadDependency(index))
    sink.entryClass := (if (index < queue.params.numEnq) 0 else
      if (queue.params.isAllComp || index >= queue.params.numEnq + queue.params.numSimp) 2 else 1).U
    sink.queue := queues.indexOf(queue).U
    entry.bits.status.srcStatus.take(3).zipWithIndex.foreach { case (source, index) =>
      sink.sourceRegister(index) := source.psrc
      sink.sourceFloating(index) := SrcType.isFp(source.srcType)
      sink.sourceUsed(index) := source.srcType.orR
      sink.sourceReady(index) := SrcState.isReady(source.srcState)
    }
  }
  timingQueueEntries.zip(memory.iqState).foreach { case ((queue, index), sink) =>
    val initial = index < queue.params.numEnq
    val current = if (initial) queue.entries.enqEntries(index).currentStatus
      else queue.entries.othersEntries(index - queue.params.numEnq).entryReg.status
    val common = if (initial) queue.entries.enqEntries(index).common
      else queue.entries.othersEntries(index - queue.params.numEnq).common
    val entry = if (initial) queue.entries.enqEntries(index).entryReg
      else queue.entries.othersEntries(index - queue.params.numEnq).entryReg
    val valid = if (initial) queue.entries.enqEntries(index).validReg
      else queue.entries.othersEntries(index - queue.params.numEnq).validReg
    sink := 0.U.asTypeOf(sink)
    sink.valid := observe(valid)
    sink.rob := observe(current.robIdx)
    sink.destination := observe(entry.payload.pdest)
    sink.queue := queues.indexOf(queue).U
    sink.initial := initial.B
    current.srcStatus.take(3).zipWithIndex.foreach { case (source, src) =>
      sink.sourceRegister(src) := observe(source.psrc)
      sink.sourceUsed(src) := observe(source.srcType).orR
      sink.sourceReady(src) := observe(source.srcState).asBool
      sink.sourceDependency(src) := observe(source.srcLoadDependency)
      sink.storedCancel(src) := observe(common.srcLoadCancelVec(src))
      sink.transferCancel(src) := observe(common.srcLoadTransCancelVec(src))
    }
  }
  incomingWakeups.zip(memory.incomingWakeup).foreach { case ((queue, source), sink) =>
    sink.valid := observe(source.valid)
    sink.integer := observe(source.bits.rfWen)
    sink.destination := observe(source.bits.pdest)
    sink.queue := queue.U
    sink.sourceLoadLane := backendParams.getLdExuIdx(source.bits.params).S(8.W)
    sink.dependency := observe(source.bits.loadDependency)
  }
  queueInputs.zip(memory.iqEnqueue).zipWithIndex.foreach { case ((source, sink), portIndex) =>
    sink := 0.U.asTypeOf(sink)
    sink.valid := observe(source.valid)
    sink.ready := observe(source.ready)
    sink.rob := observe(source.bits.robIdx)
    sink.destination := observe(source.bits.pdest)
    sink.floating := observe(source.bits.fpWen)
    val dependencies = observe(source.bits.srcLoadDependency)
    sink.dependency.zipWithIndex.foreach { case (lane, index) => lane := dependencies.map(_(index)).reduce(_ | _) }
    sink.queue := queueInputOwners(portIndex).U
    val registers = observe(source.bits.psrc)
    val types = observe(source.bits.srcType)
    val states = observe(source.bits.srcState)
    registers.take(3).zipWithIndex.foreach { case (reg, index) =>
      sink.sourceRegister(index) := reg
      sink.sourceFloating(index) := SrcType.isFp(types(index))
      sink.sourceUsed(index) := types(index).orR
      sink.sourceReady(index) := SrcState.isReady(states(index))
    }
  }
  for ((sources, sinks) <- Seq(queueDequeues -> memory.iqDequeue,
    queueDelayed -> memory.iqDelayed, dataInputs -> memory.dataInput)) {
    sources.zip(sinks).zipWithIndex.foreach { case ((source, sink), portIndex) =>
      sink := 0.U.asTypeOf(sink)
      sink.valid := observe(source.valid)
      sink.ready := observe(source.ready)
      sink.rob := observe(source.bits.common.robIdx)
      sink.destination := observe(source.bits.common.pdest)
      sink.floating := source.bits.common.fpWen.map(observe(_)).getOrElse(false.B)
      sink.dependency := source.bits.common.loadDependency.map(observe(_)).getOrElse(0.U.asTypeOf(sink.dependency))
      sink.queue := queueDequeueOwners(portIndex).U
      val types = observe(source.bits.srcType)
      val dataSources = observe(source.bits.common.dataSources)
      source.bits.rf.take(3).zipWithIndex.foreach { case (ports, index) =>
        if (ports.nonEmpty) sink.sourceRegister(index) := observe(ports.head.addr)
        sink.sourceFloating(index) := SrcType.isFp(types(index))
        sink.sourceUsed(index) := types(index).orR
        sink.registerRead(index) := dataSources(index).readReg
      }
    }
  }
  dataOutputs.zip(memory.dataOutput).foreach { case (source, sink) =>
    sink := 0.U.asTypeOf(sink)
    sink.valid := observe(source.valid)
    sink.ready := observe(source.ready)
    sink.rob := observe(source.bits.robIdx)
    sink.destination := observe(source.bits.pdest)
    sink.floating := source.bits.fpWen.map(observe(_)).getOrElse(false.B)
    sink.dependency := source.bits.loadDependency.map(observe(_)).getOrElse(0.U.asTypeOf(sink.dependency))
    sink.operand := source.bits.src.headOption.map(observe(_)).getOrElse(0.U)
  }
  queueWakeups.zip(memory.iqWakeup).foreach { case (source, sink) =>
    sink.valid := observe(source.valid)
    sink.destination := observe(source.bits.pdest)
    sink.integer := observe(source.bits.rfWen)
    sink.floating := observe(source.bits.fpWen)
    sink.dependency := observe(source.bits.loadDependency)
  }
  fpWbWakeups.zip(memory.fpWbWakeup).foreach { case ((queueIndex, source), sink) =>
    sink.valid := observe(source.valid)
    sink.floating := observe(source.bits.fpWen)
    sink.destination := observe(source.bits.pdest)
    sink.queue := queueIndex.U
  }
  (actualWriteback.io.toIntPreg.toSeq.map(_ -> false) ++ actualWriteback.io.toFpPreg.toSeq.map(_ -> true))
    .zip(memory.registerWrites).foreach { case ((source, floating), sink) =>
      sink.valid := observe(source.wen)
      sink.floating := floating.B
      sink.destination := observe(source.addr)
      sink.data := observe(source.data)
    }
  actualBypass.io.toDataPath.zip(rcSources).zip(memory.rcWrites).foreach { case ((port, source), sink) =>
    sink.valid := observe(port.wen)
    sink.data := observe(port.data)
    sink.sourceValid := observe(source.valid) && observe(source.bits.intWen)
    sink.sourceDestination := observe(source.bits.pdest)
  }
}
