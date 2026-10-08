// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.{FDIPermissionRequest, FDIPermissionResponse, PMPRespBundle}
import xiangshan.backend.fu.NewCSR.CsrTriggerBundle
import xiangshan.cache.{DCacheWordReq, DCacheWordResp, DcacheToLduForwardIO}
import xiangshan.cache.mmu._
import xiangshan.mem.Bundles._

// The load pipeline, PMM/translation and replay storage are production modules.
// Cache/forward/PMP/PTW services are public external interfaces, never forced state.
class FDILoadPermissionHarness(val remainingBoundaries: Boolean = false)(implicit p: Parameters) extends XSModule {
  private val tlbWidth = backendParams.LduCnt + backendParams.HyuCnt + 1
  val load = withReset(reset.asAsyncReset) { Module(new LoadUnit) }
  val tlb = withReset(reset.asAsyncReset) { Module(new TLBNonBlock(tlbWidth, 2, ldtlbParams)) }
  val replay = withReset(reset.asAsyncReset) { Module(new LoadQueueReplay) }
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new MemExuInput))
    val result = Decoupled(new MemExuOutput)
    val config = Option.when(HasFDI)(Input(new FDIMemoryConfig))
    val tlbCsr = Input(new TlbCsrBundle)
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val trigger = Option.when(remainingBoundaries)(Input(new CsrTriggerBundle))
    val sfence = Input(new SfenceBundle)
    val redirect = Flipped(Valid(new Redirect))
    val ptw = new TlbPtwIOwithMemIdx(tlbWidth)
    val pmp = Input(new PMPRespBundle)
    val cacheReady = Input(Bool())
    val cacheResponse = Flipped(Valid(new DCacheWordResp))
    val cacheBankConflict = Input(Bool())
    val cacheNack = Input(Bool())
    val cacheRequest = Output(Valid(new DCacheWordReq))
    val cacheS1Kill = Output(Bool())
    val cacheS2Kill = Output(Bool())
    val sq = new PipeLoadForwardQueryIO
    val sbuffer = new LoadForwardQueryIO
    val ubuffer = new LoadForwardQueryIO
    val dchannel = Input(new DcacheToLduForwardIO)
    val mshrValid = Input(Bool())
    val mshrMatch = Input(Bool())
    val mshrData = Input(Vec(VLEN / 8, UInt(8.W)))
    val mshrCorrupt = Input(Bool())
    val tlbHint = Flipped(Valid(new TLBHintResp))
    val lqHead = Input(new LqPtr)
    val phases = Output(new Bundle {
      val s0 = Bool()
      val s1 = Bool()
      val s2 = Bool()
      val s3 = Bool()
      val s0Rob = UInt(log2Ceil(RobSize).W)
      val s1Rob = UInt(log2Ceil(RobSize).W)
      val s2Rob = UInt(log2Ceil(RobSize).W)
      val s3Rob = UInt(log2Ceil(RobSize).W)
      val s2Address = UInt(XLEN.W)
      val s2TlbMiss = Bool()
      val s1NoQuery = Bool()
      val s2ForwardD = Bool()
      val s2ForwardMshr = Bool()
      val s2ForwardResult = Bool()
      val s2FullForward = Bool()
      val forwardRequest = Valid(new Bundle {
        val mshrid = UInt(load.io.forward_mshr.mshrid.getWidth.W)
        val paddr = UInt(PAddrBits.W)
      })
      val translationRetry = Bool()
      val blocked = Bool()
      val fast = Bool()
      val l2l = Bool()
      val split = Bool()
      val rollback = Bool()
      val ld1Cancel = Bool()
      val ld2Cancel = Bool()
      val replayAllocated = UInt(log2Ceil(LoadQueueReplaySize + 1).W)
      val queueWrite = Valid(new LqWriteBundle)
      val replay = Valid(new LsPipelineBundle)
      val fastReplay = Valid(new LqWriteBundle)
      val tlbResponse = Valid(new TlbResp(2))
      val permissionRequest = Valid(new FDIPermissionRequest)
      val permissionResponse = Valid(new FDIPermissionResponse)
      val permissionConsumed = Bool()
      val boundaries = Option.when(remainingBoundaries)(new Bundle {
        val tlbRefill = Bool()
        val s1Trigger = TriggerAction()
        val s2Exceptions = ExceptionVec()
        val uncacheInput = Bool()
        val uncacheEnqueue = Bool()
        val uncacheAllocated = UInt(log2Ceil(LoadUncacheBufferSize + 1).W)
        val uncacheRequest = Bool()
        val uncacheReturn = Bool()
        val uncacheRollback = Bool()
      })
    })
  })

  def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  idleInputs(load.io)
  idleInputs(tlb.io)
  idleInputs(replay.io)
  load.io.ldin <> io.request
  io.result <> load.io.ldout
  load.io.fdiConfig.foreach(_ := io.config.get)
  load.io.csrCtrl := io.csrCtrl
  io.trigger.foreach(trigger => load.io.fromCsrTrigger := trigger)
  load.io.redirect := io.redirect
  load.io.pmp := io.pmp
  load.io.dcache.req.ready := io.cacheReady
  load.io.dcache.resp.valid := io.cacheResponse.valid
  load.io.dcache.resp.bits := io.cacheResponse.bits
  load.io.dcache.s2_hit := !io.cacheResponse.bits.miss
  load.io.dcache.s2_first_hit := !io.cacheResponse.bits.miss
  load.io.dcache.s2_bank_conflict := io.cacheBankConflict
  load.io.dcache.s2_mq_nack := io.cacheNack
  io.cacheRequest.valid := load.io.dcache.req.fire
  io.cacheRequest.bits := load.io.dcache.req.bits
  io.cacheS1Kill := load.io.dcache.s1_kill
  io.cacheS2Kill := load.io.dcache.s2_kill
  io.sq <> load.io.lsq.forward
  io.sbuffer <> load.io.sbuffer
  io.ubuffer <> load.io.ubuffer
  load.io.tl_d_channel := io.dchannel
  load.io.forward_mshr.forward_result_valid := io.mshrValid
  load.io.forward_mshr.forward_mshr := io.mshrMatch
  load.io.forward_mshr.forwardData := io.mshrData
  load.io.forward_mshr.corrupt := io.mshrCorrupt
  load.io.lsq.ldld_nuke_query.req.ready := true.B
  load.io.lsq.stld_nuke_query.req.ready := true.B
  load.io.lsq.lqDeqPtr := io.lqHead
  load.io.vecldout.ready := true.B
  load.io.misalign_allow_spec := false.B
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

  replay.io.redirect := io.redirect
  replay.io.enq(0) <> load.io.lsq.ldin
  replay.io.ldWbPtr := io.lqHead
  replay.io.sqEmpty := true.B
  replay.io.stAddrReadyVec.foreach(_ := true.B)
  replay.io.stDataReadyVec.foreach(_ := true.B)
  replay.io.tlbReplayDelayCycleCtrl.foreach(_ := 0.U)
  replay.io.tlb_hint.resp := io.tlbHint
  replay.io.tl_d_channel := io.dchannel
  load.io.lq_rep_full := replay.io.lqFull
  // A single active pipeline accepts any real replay output, preserving its payload.
  // This service arbiter does not claim production cross-lane scheduling coverage.
  val replayReturn = Module(new Arbiter(new LsPipelineBundle, LoadPipelineWidth))
  replayReturn.io.in.zip(replay.io.replay).foreach { case (sink, source) => sink <> source }
  load.io.replay <> replayReturn.io.out

  if (remainingBoundaries) {
    val uncache = withReset(reset.asAsyncReset) { Module(new LoadQueueUncache) }
    idleInputs(uncache.io)
    uncache.io.redirect := io.redirect
    // Match LoadQueue's public broadcast: the replay queue alone supplies ldin.ready.
    // No permission-derived gate is inserted before the real uncache admission logic.
    uncache.io.req(0).valid := load.io.lsq.ldin.valid && !load.io.lsq.ldin.bits.nc_with_data
    uncache.io.req(0).bits := load.io.lsq.ldin.bits
    uncache.io.uncache.req.ready := true.B
    uncache.io.mmioOut.foreach(_.ready := true.B)
    uncache.io.ncOut.foreach(_.ready := true.B)
    val view = io.phases.boundaries.get
    view.tlbRefill := observe(tlb.refill)
    view.s1Trigger := observe(load.s1_out.uop.trigger)
    view.s2Exceptions := observe(load.s2_in.uop.exceptionVec)
    view.uncacheInput := uncache.io.req(0).valid
    view.uncacheEnqueue := VecInit(uncache.s2_enqValidVec.map(bit => observe(bit))).asUInt.orR
    view.uncacheAllocated := PopCount(VecInit(uncache.entries.map(e => observe(e.req_valid))))
    view.uncacheRequest := uncache.io.uncache.req.valid
    view.uncacheReturn := uncache.io.mmioOut.map(_.valid).reduce(_ || _) ||
      uncache.io.ncOut.map(_.valid).reduce(_ || _)
    view.uncacheRollback := uncache.io.rollback.valid
  }

  io.phases.s0 := observe(load.s0_fire)
  io.phases.s1 := observe(load.s1_fire)
  io.phases.s2 := observe(load.s2_fire)
  io.phases.s3 := observe(load.s3_valid)
  io.phases.s0Rob := observe(load.s0_out.uop.robIdx.value)
  io.phases.s1Rob := observe(load.s1_out.uop.robIdx.value)
  io.phases.s2Rob := observe(load.s2_in.uop.robIdx.value)
  io.phases.s3Rob := observe(load.s3_in.uop.robIdx.value)
  io.phases.s2Address := observe(load.s2_in.fullva)
  io.phases.s2TlbMiss := observe(load.s2_in.tlbMiss)
  io.phases.s1NoQuery := observe(load.s1_in.tlbNoQuery)
  io.phases.s2ForwardD := observe(load.s2_fwd_frm_d_chan)
  io.phases.s2ForwardMshr := observe(load.s2_fwd_frm_mshr)
  io.phases.s2ForwardResult := observe(load.s2_fwd_data_valid)
  io.phases.s2FullForward := observe(load.s2_full_fwd)
  io.phases.forwardRequest.valid := load.io.forward_mshr.valid
  io.phases.forwardRequest.bits.mshrid := load.io.forward_mshr.mshrid
  io.phases.forwardRequest.bits.paddr := load.io.forward_mshr.paddr
  io.phases.translationRetry := observe(load.s2_fdiTranslationRetry)
  io.phases.blocked := (if (HasFDI) observe(load.s2_fdiBlocked) else false.B)
  io.phases.fast := load.io.fast_uop.valid
  io.phases.l2l := load.io.l2l_fwd_out.valid
  io.phases.split := load.io.misalign_enq.req.valid
  io.phases.rollback := load.io.rollback.valid
  io.phases.ld1Cancel := load.io.ldCancel.ld1Cancel
  io.phases.ld2Cancel := load.io.ldCancel.ld2Cancel
  io.phases.replayAllocated := PopCount(observe(replay.allocated))
  io.phases.queueWrite.valid := load.io.lsq.ldin.fire
  io.phases.queueWrite.bits := load.io.lsq.ldin.bits
  io.phases.replay.valid := replayReturn.io.out.fire
  io.phases.replay.bits := replayReturn.io.out.bits
  io.phases.fastReplay.valid := load.io.fast_rep_out.fire
  io.phases.fastReplay.bits := load.io.fast_rep_out.bits
  io.phases.tlbResponse.valid := load.io.tlb.resp.valid
  io.phases.tlbResponse.bits := load.io.tlb.resp.bits
  io.phases.permissionRequest := 0.U.asTypeOf(io.phases.permissionRequest)
  io.phases.permissionResponse := 0.U.asTypeOf(io.phases.permissionResponse)
  io.phases.permissionConsumed := false.B
  load.fdiPermission.foreach { checker =>
    io.phases.permissionRequest.valid := observe(checker.io.req.valid) && observe(checker.io.req.ready)
    io.phases.permissionRequest.bits := observe(checker.io.req.bits)
    io.phases.permissionResponse.valid := observe(checker.io.resp.valid)
    io.phases.permissionResponse.bits := observe(checker.io.resp.bits)
    io.phases.permissionConsumed := observe(checker.io.resp.valid) && observe(checker.io.resp.ready)
  }
}
