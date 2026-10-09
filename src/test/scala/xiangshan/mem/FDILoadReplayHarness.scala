// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.{FDIPermissionRequest, FDIPermissionResponse}
import xiangshan.cache.{DCacheWordReq, DCacheWordResp}
import xiangshan.cache.mmu.TLBHintResp
import xiangshan.mem.Bundles._

// Three production pipelines retain the production queue-to-lane routing.
// Translation and cache responses are public services; no internal state is forced.
class FDILoadReplayHarness(implicit p: Parameters) extends XSModule {
  require(backendParams.LduCnt == 3 && backendParams.HyuCnt == 0)
  require(HasVPU && VLEN == 128 && !EnableLoadToLoadForward)
  val loads = Seq.fill(3)(withReset(reset.asAsyncReset) { Module(new LoadUnit) })
  val queue = withReset(reset.asAsyncReset) { Module(new LoadQueueReplay) }

  class Lane extends Bundle {
    val request = Flipped(Decoupled(new MemExuInput))
    val result = Decoupled(new MemExuOutput)
    val cacheReady = Input(Bool())
    val response = Flipped(Valid(new DCacheWordResp))
    val bankConflict = Input(Bool())
    val cacheNack = Input(Bool())
    val wpuFailure = Input(Bool())
    val rawReady = Input(Bool())
    val rarReady = Input(Bool())
    // Translation outcomes are attached to the accepted S1 request. The service
    // does not model a PTW; queue wakeup uses the production hint interface.
    val tlbMiss = Input(Bool())
    val tlbPageFault = Input(Bool())
    val nuke = Flipped(Valid(new StoreNukeQueryBundle))
    val sq = new PipeLoadForwardQueryIO
    val sbuffer = new LoadForwardQueryIO
    val cacheRequest = Output(Valid(new DCacheWordReq))
    val cacheRob = Output(UInt(log2Ceil(RobSize).W))
    val cacheRobFlag = Output(Bool())
    val cacheS1Kill = Output(Bool())
    val cacheS2Kill = Output(Bool())
    val stage1 = Output(Valid(new LsPipelineBundle))
    val stage2 = Output(Valid(new LsPipelineBundle))
    val permissionRequest = Output(Valid(new FDIPermissionRequest))
    val permissionResponse = Output(Valid(new FDIPermissionResponse))
    val queueWrite = Output(Valid(new LqWriteBundle))
    val replay = Output(Valid(new LsPipelineBundle))
    val replayReady = Output(Bool())
    val fastReplay = Output(Valid(new LqWriteBundle))
    val fastReady = Output(Bool())
    val blocked = Output(Bool())
    val fastWakeup = Output(Bool())
    val rollback = Output(Bool())
    val cancel1 = Output(Bool())
    val cancel2 = Output(Bool())
  }
  val io = IO(new Bundle {
    val lanes = Vec(3, new Lane)
    val config = Option.when(HasFDI)(Input(new FDIMemoryConfig))
    val redirect = Flipped(Valid(new Redirect))
    val tlbHint = Flipped(Valid(new TLBHintResp))
    val rarFull = Input(Bool())
    val rawFull = Input(Bool())
    val sqEmpty = Input(Bool())
    val storeAddressReady = Input(Bool())
    val allocated = Output(UInt(log2Ceil(LoadQueueReplaySize + 1).W))
    val allocationMask = Output(UInt(LoadQueueReplaySize.W))
    val freeMask = Output(UInt(LoadQueueReplaySize.W))
  })

  private def idle(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idle)
    case vector: Vec[_] => vector.foreach(idle)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  idle(queue.io)
  queue.io.redirect := io.redirect
  queue.io.sqEmpty := io.sqEmpty
  queue.io.stAddrReadyVec.foreach(_ := io.storeAddressReady)
  queue.io.stDataReadyVec.foreach(_ := true.B)
  queue.io.tlbReplayDelayCycleCtrl.foreach(_ := 0.U)
  queue.io.tlb_hint.resp := io.tlbHint
  queue.io.rarFull := io.rarFull
  queue.io.rawFull := io.rawFull

  loads.zip(io.lanes).zipWithIndex.foreach { case ((load, lane), index) =>
    idle(load.io)
    load.io.ldin <> lane.request
    lane.result <> load.io.ldout
    load.io.redirect := io.redirect
    load.io.fdiConfig.foreach(_ := io.config.get)
    load.io.csrCtrl.cache_error_enable := true.B
    load.io.csrCtrl.hd_misalign_ld_enable := true.B
    load.io.vecldout.ready := true.B
    load.io.dcache.req.ready := lane.cacheReady
    load.io.dcache.resp.valid := lane.response.valid
    load.io.dcache.resp.bits := lane.response.bits
    load.io.dcache.s2_hit := !lane.response.bits.miss
    load.io.dcache.s2_first_hit := !lane.response.bits.miss
    load.io.dcache.s2_bank_conflict := lane.bankConflict
    load.io.dcache.s2_mq_nack := lane.cacheNack
    load.io.dcache.s2_wpu_pred_fail := lane.wpuFailure
    load.io.lsq.forward <> lane.sq
    load.io.sbuffer <> lane.sbuffer
    load.io.lsq.stld_nuke_query.req.ready := lane.rawReady
    load.io.lsq.ldld_nuke_query.req.ready := lane.rarReady
    load.io.stld_nuke_query.head := lane.nuke
    load.io.fast_rep_in <> load.io.fast_rep_out
    load.io.replay <> queue.io.replay(index)
    queue.io.enq(index) <> load.io.lsq.ldin
    load.io.lq_rep_full := queue.io.lqFull

    // The translated service echoes the accepted full VA and queue identity.
    // It models a stable mapped page; PMM and page-walk semantics retain their L01 tests.
    load.io.tlb.req.ready := true.B
    val translation = RegEnable(load.io.tlb.req.bits, load.io.tlb.req.fire)
    load.io.tlb.resp.valid := RegNext(load.io.tlb.req.fire && !load.io.tlb.req.bits.kill, false.B)
    load.io.tlb.resp.bits.fullva := translation.fullva
    load.io.tlb.resp.bits.memidx := translation.memidx
    load.io.tlb.resp.bits.debug.robIdx := translation.debug.robIdx
    load.io.tlb.resp.bits.debug.isFirstIssue := translation.debug.isFirstIssue
    load.io.tlb.resp.bits.paddr.foreach(_ := translation.fullva(PAddrBits - 1, 0))
    load.io.tlb.resp.bits.gpaddr.foreach(_ := translation.fullva)
    load.io.tlb.resp.bits.miss := lane.tlbMiss
    load.io.tlb.resp.bits.fastMiss := lane.tlbMiss
    load.io.tlb.resp.bits.excp.foreach(_.pf.ld := lane.tlbPageFault)

    lane.cacheRequest.valid := load.io.dcache.req.fire
    lane.cacheRequest.bits := load.io.dcache.req.bits
    lane.cacheRob := observe(load.s0_out.uop.robIdx.value)
    lane.cacheRobFlag := observe(load.s0_out.uop.robIdx.flag)
    lane.cacheS1Kill := load.io.dcache.s1_kill
    lane.cacheS2Kill := load.io.dcache.s2_kill
    lane.stage1.valid := observe(load.s1_fire)
    lane.stage1.bits := observe(load.s1_out)
    lane.stage2.valid := observe(load.s2_fire)
    lane.stage2.bits := observe(load.s2_in)
    lane.queueWrite.valid := load.io.lsq.ldin.fire
    lane.queueWrite.bits := load.io.lsq.ldin.bits
    lane.replay.valid := queue.io.replay(index).valid
    lane.replay.bits := queue.io.replay(index).bits
    lane.replayReady := queue.io.replay(index).ready
    lane.fastReplay.valid := load.io.fast_rep_out.valid
    lane.fastReplay.bits := load.io.fast_rep_out.bits
    lane.fastReady := load.io.fast_rep_out.ready
    lane.blocked := (if (HasFDI) observe(load.s2_fdiBlocked) else false.B)
    lane.fastWakeup := load.io.fast_uop.valid
    lane.rollback := load.io.rollback.valid
    lane.cancel1 := load.io.ldCancel.ld1Cancel
    lane.cancel2 := load.io.ldCancel.ld2Cancel
    lane.permissionRequest := 0.U.asTypeOf(lane.permissionRequest)
    lane.permissionResponse := 0.U.asTypeOf(lane.permissionResponse)
    load.fdiPermission.foreach { checker =>
      lane.permissionRequest.valid := observe(checker.io.req.valid) && observe(checker.io.req.ready)
      lane.permissionRequest.bits := observe(checker.io.req.bits)
      lane.permissionResponse.valid := observe(checker.io.resp.valid) && observe(checker.io.resp.ready)
      lane.permissionResponse.bits := observe(checker.io.resp.bits)
    }
  }
  io.allocated := PopCount(observe(queue.allocated))
  io.allocationMask := observe(queue.allocated).asUInt
  io.freeMask := observe(queue.freeMaskVec).asUInt
}
