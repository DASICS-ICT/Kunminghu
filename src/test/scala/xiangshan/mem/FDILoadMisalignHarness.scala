// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.{FDIPermissionRequest, FDIPermissionResponse, PMPChecker, PMPCheckerEnv}
import xiangshan.cache.{DCacheWordReq, DCacheWordResp}
import xiangshan.cache.mmu._
import xiangshan.mem.Bundles._

// Translation, permission, splitting, replay and exception selection are production logic.
// The service boundary supplies cache replies, PTW replies and PMP/PMA configuration only.
class FDILoadMisalignHarness(implicit p: Parameters) extends XSModule {
  private val tlbWidth = backendParams.LduCnt + backendParams.HyuCnt + 1
  val load = withReset(reset.asAsyncReset) { Module(new LoadUnit) }
  val split = withReset(reset.asAsyncReset) { Module(new LoadMisalignBuffer) }
  val tlb = withReset(reset.asAsyncReset) { Module(new TLBNonBlock(tlbWidth, 2, ldtlbParams)) }
  val protection = withReset(reset.asAsyncReset) { Module(new PMPChecker(4, leaveHitMux = true)) }
  val replay = withReset(reset.asAsyncReset) { Module(new LoadQueueReplay) }
  val exceptions = withReset(reset.asAsyncReset) { Module(new LqExceptionBuffer) }
  val uncache = withReset(reset.asAsyncReset) { Module(new LoadQueueUncache) }

  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new MemExuInput))
    val result = Decoupled(new MemExuOutput)
    val config = Option.when(HasFDI)(Input(new FDIMemoryConfig))
    val tlbCsr = Input(new TlbCsrBundle)
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val pmpEnvironment = Input(new PMPCheckerEnv)
    val sfence = Input(new SfenceBundle)
    val redirect = Flipped(Valid(new Redirect))
    val ptw = new TlbPtwIOwithMemIdx(tlbWidth)
    val tlbHint = Flipped(Valid(new TLBHintResp))
    val cacheReady = Input(Bool())
    val cacheResponse = Flipped(Valid(new DCacheWordResp))
    val cacheBankConflict = Input(Bool())
    val cacheNack = Input(Bool())
    val cacheRequest = Output(Valid(new DCacheWordReq))
    val cacheS1Kill = Output(Bool())
    val cacheS2Kill = Output(Bool())
    val phases = Output(new Bundle {
      val s0Rob = UInt(log2Ceil(RobSize).W)
      val s0FromMisalign = Bool()
      val s1 = Bool()
      val s2 = Bool()
      val s3 = Bool()
      val s2Rob = UInt(log2Ceil(RobSize).W)
      val s2Address = UInt(XLEN.W)
      val s2TlbMiss = Bool()
      val s2Blocked = Bool()
      val splitEnqueue = Valid(new LqWriteBundle)
      val splitRequest = Valid(new LsPipelineBundle)
      val splitBackpressured = Bool()
      val splitResponse = Valid(new LqWriteBundle)
      val splitWriteback = Valid(new MemExuOutput)
      val normalWriteback = Valid(UInt(log2Ceil(RobSize).W))
      val splitWritebackReady = Bool()
      val splitExceptionPending = Bool()
      val splitSavedRob = chiselTypeOf(split.io.writeBack.bits.uop.robIdx)
      val splitSavedLq = chiselTypeOf(split.io.writeBack.bits.uop.lqIdx)
      val splitSavedDestination = UInt(PhyRegIdxWidth.W)
      val splitSavedPc = UInt(VAddrBits.W)
      val splitSavedAddress = UInt(XLEN.W)
      val splitSavedException = ExceptionVec()
      val splitState = UInt(3.W)
      val splitFull = Bool()
      val splitOwner = UInt(log2Ceil(RobSize).W)
      val queueWrite = Valid(new LqWriteBundle)
      val replay = Valid(new LsPipelineBundle)
      val replayAllocated = UInt(log2Ceil(LoadQueueReplaySize + 1).W)
      val fastReplay = Valid(new LqWriteBundle)
      val tlbRequest = Valid(chiselTypeOf(load.io.tlb.req.bits))
      val tlbResponse = Valid(new TlbResp(2))
      val pmpRequest = Valid(chiselTypeOf(protection.io.req.bits))
      val pmpFault = Bool()
      val exceptionValid = Bool()
      val exceptionOwner = UInt(log2Ceil(RobSize).W)
      val exceptionAddress = UInt(XLEN.W)
      val permissionRequest = Valid(new FDIPermissionRequest)
      val permissionResponse = Valid(new FDIPermissionResponse)
      val ordinaryWakeup = Bool()
      val fastWakeup = Bool()
      val sqForward = Bool()
      val sbufferForward = Bool()
      val mshrForward = Bool()
      val uncacheForward = Bool()
      val uncacheEnqueue = Bool()
      val uncacheAllocated = UInt(log2Ceil(LoadUncacheBufferSize + 1).W)
      val uncacheRequest = Bool()
      val uncacheRequestAddress = UInt(PAddrBits.W)
      val l2lForward = Bool()
      val rollback = Bool()
    })
  })

  private def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  Seq(load.io, split.io, tlb.io, protection.io, replay.io, exceptions.io, uncache.io).foreach(idleInputs)

  load.io.ldin <> io.request
  load.io.redirect := io.redirect
  load.io.csrCtrl := io.csrCtrl
  load.io.fdiConfig.foreach(_ := io.config.get)
  load.io.dcache.req.ready := io.cacheReady
  load.io.dcache.resp.valid := io.cacheResponse.valid
  load.io.dcache.resp.bits := io.cacheResponse.bits
  load.io.dcache.s2_hit := !io.cacheResponse.bits.miss
  load.io.dcache.s2_first_hit := !io.cacheResponse.bits.miss
  load.io.dcache.s2_bank_conflict := io.cacheBankConflict
  load.io.dcache.s2_mq_nack := io.cacheNack
  load.io.lsq.ldld_nuke_query.req.ready := true.B
  load.io.lsq.stld_nuke_query.req.ready := true.B
  load.io.vecldout.ready := true.B
  load.io.misalign_allow_spec := true.B
  load.io.fast_rep_in <> load.io.fast_rep_out

  tlb.io.csr := io.tlbCsr
  tlb.io.sfence := io.sfence
  tlb.io.redirect := io.redirect
  tlb.io.requestor.foreach(_.resp.ready := true.B)
  tlb.io.requestor(0) <> load.io.tlb
  io.ptw <> tlb.io.ptw
  if (ldtlbParams.outReplace) {
    val replacement = Module(new TlbReplace(tlbWidth, ldtlbParams))
    replacement.io.apply_sep(Seq(tlb.io.replace), io.ptw.resp.bits.s1.entry.tag)
  }
  protection.io.check_env := io.pmpEnvironment
  protection.io.req := tlb.io.pmp(0)
  load.io.pmp := protection.io.resp

  replay.io.redirect := io.redirect
  replay.io.enq(0) <> load.io.lsq.ldin
  replay.io.sqEmpty := true.B
  replay.io.stAddrReadyVec.foreach(_ := true.B)
  replay.io.stDataReadyVec.foreach(_ := true.B)
  replay.io.tlbReplayDelayCycleCtrl.foreach(_ := 0.U)
  replay.io.tlb_hint.resp := io.tlbHint
  replay.io.loadMisalignFull := split.io.loadMisalignFull
  replay.io.misalignAllowSpec := true.B
  load.io.lq_rep_full := replay.io.lqFull
  val replayReturn = Module(new Arbiter(new LsPipelineBundle, LoadPipelineWidth))
  replayReturn.io.in.zip(replay.io.replay).foreach { case (sink, source) => sink <> source }
  load.io.replay <> replayReturn.io.out

  split.io.redirect := io.redirect
  split.io.enq(0) <> load.io.misalign_enq
  load.io.misalign_ldin <> split.io.splitLoadReq
  split.io.splitLoadResp := load.io.misalign_ldout
  split.io.vecWriteBack.ready := true.B
  // Match the production shared writeback port, including normal-load priority.
  io.result.valid := load.io.ldout.valid || split.io.writeBack.valid
  io.result.bits := Mux(load.io.ldout.valid, load.io.ldout.bits, split.io.writeBack.bits)
  io.result.bits.isFromLoadUnit := load.io.ldout.bits.isFromLoadUnit || split.io.writeBack.valid
  load.io.ldout.ready := io.result.ready
  split.io.writeBack.ready := io.result.ready && !load.io.ldout.valid
  split.io.loadOutValid := load.io.ldout.valid
  split.io.loadVecOutValid := load.io.vecldout.valid

  exceptions.io.redirect := io.redirect
  exceptions.io.req(0).valid := load.io.lsq.ldin.valid && !load.io.lsq.ldin.bits.isvec
  exceptions.io.req(0).bits := load.io.lsq.ldin.bits

  // Match the native LoadQueue broadcast without test-side filtering of NC fragments.
  uncache.io.redirect := io.redirect
  uncache.io.req(0).valid := load.io.lsq.ldin.valid && !load.io.lsq.ldin.bits.nc_with_data
  uncache.io.req(0).bits := load.io.lsq.ldin.bits
  uncache.io.uncache.req.ready := true.B
  uncache.io.mmioOut.foreach(_.ready := true.B)
  uncache.io.ncOut.foreach(_.ready := true.B)

  io.cacheRequest.valid := load.io.dcache.req.fire
  io.cacheRequest.bits := load.io.dcache.req.bits
  io.cacheS1Kill := load.io.dcache.s1_kill
  io.cacheS2Kill := load.io.dcache.s2_kill
  val view = io.phases
  view.s0Rob := observe(load.s0_out.uop.robIdx.value)
  view.s0FromMisalign := observe(load.s0_out.isFrmMisAlignBuf)
  view.s1 := observe(load.s1_fire)
  view.s2 := observe(load.s2_fire)
  view.s3 := observe(load.s3_valid)
  view.s2Rob := observe(load.s2_in.uop.robIdx.value)
  view.s2Address := observe(load.s2_in.fullva)
  view.s2TlbMiss := observe(load.s2_in.tlbMiss)
  view.s2Blocked := (if (HasFDI) observe(load.s2_fdiBlocked) else false.B)
  view.splitEnqueue.valid := split.io.enq(0).req.fire
  view.splitEnqueue.bits := split.io.enq(0).req.bits
  view.splitRequest.valid := split.io.splitLoadReq.fire
  view.splitRequest.bits := split.io.splitLoadReq.bits
  view.splitBackpressured := split.io.splitLoadReq.valid && !split.io.splitLoadReq.ready
  view.splitResponse := split.io.splitLoadResp
  view.splitWriteback.valid := split.io.writeBack.fire
  view.splitWriteback.bits := split.io.writeBack.bits
  view.normalWriteback.valid := load.io.ldout.valid
  view.normalWriteback.bits := load.io.ldout.bits.uop.robIdx.value
  view.splitWritebackReady := split.io.writeBack.ready
  // Observe saved ownership independently of the normal-load-priority output mask.
  view.splitExceptionPending := observe(split.req_valid) && observe(split.globalException) &&
    (observe(split.bufferState) === split.s_wb)
  view.splitSavedRob := observe(split.req.uop.robIdx)
  view.splitSavedLq := observe(split.req.uop.lqIdx)
  view.splitSavedDestination := observe(split.req.uop.pdest)
  view.splitSavedPc := observe(split.req.uop.pc)
  view.splitSavedAddress := observe(split.req.fullva)
  view.splitSavedException := observe(split.exceptionVec)
  view.splitState := observe(split.bufferState)
  view.splitFull := split.io.loadMisalignFull
  view.splitOwner := observe(split.req.uop.robIdx.value)
  view.queueWrite.valid := load.io.lsq.ldin.fire
  view.queueWrite.bits := load.io.lsq.ldin.bits
  view.replay.valid := replayReturn.io.out.fire
  view.replay.bits := replayReturn.io.out.bits
  view.replayAllocated := PopCount(observe(replay.allocated))
  view.fastReplay.valid := load.io.fast_rep_out.fire
  view.fastReplay.bits := load.io.fast_rep_out.bits
  view.tlbRequest.valid := load.io.tlb.req.fire
  view.tlbRequest.bits := load.io.tlb.req.bits
  view.tlbResponse.valid := load.io.tlb.resp.valid
  view.tlbResponse.bits := load.io.tlb.resp.bits
  view.pmpRequest := protection.io.req
  view.pmpFault := protection.io.resp.ld
  view.exceptionValid := observe(exceptions.req_valid)
  view.exceptionOwner := observe(exceptions.req.uop.robIdx.value)
  view.exceptionAddress := Mux(split.io.overwriteExpBuf.valid,
    split.io.overwriteExpBuf.vaddr, exceptions.io.exceptionAddr.vaddr)
  view.permissionRequest := 0.U.asTypeOf(view.permissionRequest)
  view.permissionResponse := 0.U.asTypeOf(view.permissionResponse)
  load.fdiPermission.foreach { checker =>
    view.permissionRequest.valid := observe(checker.io.req.valid) && observe(checker.io.req.ready)
    view.permissionRequest.bits := observe(checker.io.req.bits)
    view.permissionResponse.valid := observe(checker.io.resp.valid) && observe(checker.io.resp.ready)
    view.permissionResponse.bits := observe(checker.io.resp.bits)
  }
  view.ordinaryWakeup := load.io.wakeup.valid
  view.fastWakeup := load.io.fast_uop.valid
  view.sqForward := load.io.lsq.forward.valid
  view.sbufferForward := load.io.sbuffer.valid
  view.mshrForward := load.io.forward_mshr.valid
  view.uncacheForward := load.io.ubuffer.valid
  view.uncacheEnqueue := VecInit(uncache.s2_enqValidVec.map(bit => observe(bit))).asUInt.orR
  view.uncacheAllocated := PopCount(VecInit(uncache.entries.map(entry => observe(entry.req_valid))))
  view.uncacheRequest := uncache.io.uncache.req.valid
  view.uncacheRequestAddress := uncache.io.uncache.req.bits.addr
  view.l2lForward := load.io.l2l_fwd_out.valid
  view.rollback := load.io.rollback.valid
}
