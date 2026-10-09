// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.experimental.UnlocatableSourceInfo
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, RegionType, TransferSizes}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.{FDIPermissionRequest, FDIPermissionResponse}
import xiangshan.cache._
import xiangshan.mem.Bundles._

// Cold cache arrays and identity translation are public services. Miss admission,
// grant storage, forwarding, replay ownership and terminal publication are real.
// One active load pipeline covers this lifecycle; cross-lane scheduling is separate.
class FDILoadReplayMissHarness(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  require(backendParams.LduCnt == 3 && backendParams.HyuCnt == 0)
  require(HasVPU && VLEN == 128 && !EnableLoadToLoadForward)
  require(cfg.blockBytes == 64 && l1BusDataWidth == 256)

  private val cacheOuter = LazyModule(new DCache)
  private val manager = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, (BigInt(1) << PAddrBits) - 1)),
      regionType = RegionType.CACHED,
      supportsAcquireB = TransferSizes(cfg.blockBytes),
      supportsAcquireT = TransferSizes(cfg.blockBytes),
      supportsGet = TransferSizes(1, cfg.blockBytes))),
    beatBytes = l1BusDataWidth / 8, endSinkId = 1, minLatency = 1,
    requestKeys = cacheOuter.clientParameters.requestFields.map(_.key))
  private val edge = new TLEdgeOut(cacheOuter.clientParameters, manager, p, UnlocatableSourceInfo)

  val load = withReset(reset.asAsyncReset) { Module(new LoadUnit) }
  val pipe = withReset(reset.asAsyncReset) { Module(new LoadPipe(0)) }
  val miss = withReset(reset.asAsyncReset) { Module(new MissQueue(edge, 1)) }
  val replay = withReset(reset.asAsyncReset) { Module(new LoadQueueReplay) }
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new MemExuInput))
    val config = Option.when(HasFDI)(Input(new FDIMemoryConfig))
    val redirect = Flipped(Valid(new Redirect))
    val acquireReady = Input(Bool())
    val grant = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val hint = Flipped(Valid(new L2ToL1Hint))
    // The array-install service acknowledges only after this trace has consumed
    // outstanding loads, keeping real MSHR forwarding data available until then.
    val finishEnable = Input(Bool())
    val completion = Output(Valid(new MemExuOutput))
    val queueUpdate = Output(Valid(new LqWriteBundle))
    val replayRequest = Output(Valid(new LsPipelineBundle))
    val permissionRequest = Output(Valid(new FDIPermissionRequest))
    val permissionResponse = Output(Valid(new FDIPermissionResponse))
    val acquire = Output(Valid(new TLBundleA(edge.bundle)))
    val refill = Output(Valid(new Refill))
    val grantAck = Output(Bool())
    val allocation = Output(Bool())
    val merge = Output(Bool())
    val updateEntry = Output(UInt(log2Ceil(cfg.nMissEntries).W))
    val updateAddress = Output(UInt(PAddrBits.W))
    val rawMiss = Output(Valid(new MissReq))
    val replayAllocated = Output(UInt(log2Ceil(LoadQueueReplaySize + 1).W))
    val entryValid = Output(Vec(cfg.nMissEntries, Bool()))
    val forwardRequest = Output(Valid(new Bundle {
      val mshrid = UInt(log2Ceil(cfg.nMissEntries).W)
      val paddr = UInt(PAddrBits.W)
    }))
    val s2Forward = Output(new Bundle {
      val valid = Bool()
      val rob = UInt(log2Ceil(RobSize).W)
      val fullva = UInt(XLEN.W)
      val mshr = Bool()
      val dchannel = Bool()
      val resultValid = Bool()
    })
  })

  private def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  Seq(load.io, pipe.io, miss.io, replay.io).foreach(idleInputs)
  load.io.ldin <> io.request
  load.io.fdiConfig.zip(io.config).foreach { case (sink, source) => sink := source }
  load.io.redirect := io.redirect
  load.io.ldout.ready := true.B
  load.io.vecldout.ready := true.B
  load.io.lsq.ldld_nuke_query.req.ready := true.B
  load.io.lsq.stld_nuke_query.req.ready := true.B
  load.io.misalign_enq.req.ready := true.B
  load.io.fast_rep_in <> load.io.fast_rep_out

  load.io.tlb.req.ready := true.B
  val translation = RegEnable(load.io.tlb.req.bits, load.io.tlb.req.fire)
  load.io.tlb.resp.valid := RegNext(load.io.tlb.req.fire && !load.io.tlb.req.bits.kill, false.B)
  load.io.tlb.resp.bits.fullva := translation.fullva
  load.io.tlb.resp.bits.memidx := translation.memidx
  load.io.tlb.resp.bits.debug.robIdx := translation.debug.robIdx
  load.io.tlb.resp.bits.debug.isFirstIssue := translation.debug.isFirstIssue
  load.io.tlb.resp.bits.paddr.foreach(_ := translation.fullva(PAddrBits - 1, 0))
  load.io.tlb.resp.bits.gpaddr.foreach(_ := translation.fullva)

  pipe.io.lsu <> load.io.dcache
  pipe.io.load128Req := load.io.dcache.is128Req
  pipe.io.meta_read.ready := true.B
  pipe.io.tag_read.ready := true.B
  pipe.io.banked_data_read.ready := true.B
  pipe.io.access_flag_write.ready := true.B
  pipe.io.prefetch_flag_write.ready := true.B
  miss.io.req <> pipe.io.miss_req
  pipe.io.miss_resp := miss.io.resp
  miss.io.queryMQ.head.req.valid := pipe.io.miss_req.valid
  miss.io.queryMQ.head.req.bits := pipe.io.miss_req.bits
  miss.io.mem_acquire.ready := io.acquireReady
  miss.io.mem_grant <> io.grant
  miss.io.mem_finish.ready := true.B
  miss.io.cmo_resp.ready := true.B
  miss.io.l2_hint := io.hint
  miss.io.forward.head <> load.io.forward_mshr

  // Use the same public GrantData decoding as DCacheWrapper, including beat
  // selection, source ID and corrupt/denied metadata. No forward result is injected.
  val dchannel = Wire(new DcacheToLduForwardIO)
  when (io.grant.bits.opcode === TLMessages.GrantData) {
    dchannel.apply(io.grant, edge)
  }.otherwise {
    dchannel.dontCare()
  }
  load.io.tl_d_channel := dchannel
  replay.io.tl_d_channel := dchannel
  replay.io.l2_hint := io.hint
  replay.io.redirect := io.redirect
  replay.io.enq.head <> load.io.lsq.ldin
  replay.io.sqEmpty := true.B
  replay.io.stAddrReadyVec.foreach(_ := true.B)
  replay.io.stDataReadyVec.foreach(_ := true.B)
  replay.io.tlbReplayDelayCycleCtrl.foreach(_ := 0.U)
  load.io.lq_rep_full := replay.io.lqFull
  val returns = Module(new Arbiter(new LsPipelineBundle, LoadPipelineWidth))
  returns.io.in.zip(replay.io.replay).foreach { case (sink, source) => sink <> source }
  load.io.replay <> returns.io.out

  miss.io.main_pipe_req.ready := io.finishEnable
  val install = RegNext(miss.io.main_pipe_req.fire, false.B)
  val installId = RegEnable(miss.io.main_pipe_req.bits.miss_id, miss.io.main_pipe_req.fire)
  miss.io.main_pipe_resp.valid := install
  miss.io.main_pipe_resp.bits.ack_miss_queue := true.B
  miss.io.main_pipe_resp.bits.miss_id := installId
  miss.io.mainpipe_info.s3_valid := RegNext(install, false.B)
  miss.io.mainpipe_info.s3_refill_resp := true.B
  miss.io.mainpipe_info.s3_miss_id := RegNext(installId)

  io.completion.valid := load.io.ldout.fire
  io.completion.bits := load.io.ldout.bits
  io.queueUpdate.valid := load.io.lsq.ldin.fire
  io.queueUpdate.bits := load.io.lsq.ldin.bits
  io.replayRequest.valid := returns.io.out.fire
  io.replayRequest.bits := returns.io.out.bits
  io.permissionRequest := 0.U.asTypeOf(io.permissionRequest)
  io.permissionResponse := 0.U.asTypeOf(io.permissionResponse)
  load.fdiPermission.foreach { checker =>
    io.permissionRequest.valid := observe(checker.io.req.valid) && observe(checker.io.req.ready)
    io.permissionRequest.bits := observe(checker.io.req.bits)
    io.permissionResponse.valid := observe(checker.io.resp.valid) && observe(checker.io.resp.ready)
    io.permissionResponse.bits := observe(checker.io.resp.bits)
  }
  io.acquire.valid := miss.io.mem_acquire.fire
  io.acquire.bits := miss.io.mem_acquire.bits
  io.refill := miss.io.refill_to_ldq
  io.grantAck := miss.io.mem_finish.fire
  io.allocation := observe(miss.miss_req_pipe_reg.alloc)
  io.merge := observe(miss.miss_req_pipe_reg.merge)
  io.updateEntry := observe(miss.miss_req_pipe_reg.mshr_id)
  io.updateAddress := observe(miss.miss_req_pipe_reg.req.addr)
  io.rawMiss.valid := miss.io.req.valid
  io.rawMiss.bits := miss.io.req.bits
  io.replayAllocated := PopCount(observe(replay.allocated))
  miss.entries.zipWithIndex.foreach { case (entry, index) => io.entryValid(index) := observe(entry.req_valid) }
  io.forwardRequest.valid := load.io.forward_mshr.valid
  io.forwardRequest.bits.mshrid := load.io.forward_mshr.mshrid
  io.forwardRequest.bits.paddr := load.io.forward_mshr.paddr
  io.s2Forward.valid := observe(load.s2_valid)
  io.s2Forward.rob := observe(load.s2_in.uop.robIdx.value)
  io.s2Forward.fullva := observe(load.s2_in.fullva)
  io.s2Forward.mshr := observe(load.s2_fwd_frm_mshr)
  io.s2Forward.dchannel := observe(load.s2_fwd_frm_d_chan)
  io.s2Forward.resultValid := observe(load.s2_fwd_data_valid)
}
