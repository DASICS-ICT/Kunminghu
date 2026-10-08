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
import xiangshan.mem.Bundles.{FDIMemoryConfig, LqWriteBundle}

// This cutpoint keeps the production load, permission, cache pipeline and MSHRs.
// Translation is an identity service; cold array responses never install a line.
class FDILoadPermissionMissHarness(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  require(backendParams.LduCnt == 3 && backendParams.HyuCnt == 0)
  require(HasVPU && VLEN == 128 && !EnableLoadToLoadForward)

  // Reuse the production master's source range and request/echo fields verbatim.
  // The external manager delays all grants throughout this bounded admission test.
  private val cacheOuter = LazyModule(new DCache)
  private val manager = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, (BigInt(1) << PAddrBits) - 1)),
      regionType = RegionType.CACHED,
      supportsAcquireB = TransferSizes(cfg.blockBytes),
      supportsAcquireT = TransferSizes(cfg.blockBytes),
      supportsGet = TransferSizes(1, cfg.blockBytes))),
    beatBytes = l1BusDataWidth / 8,
    endSinkId = 1,
    minLatency = 1,
    requestKeys = cacheOuter.clientParameters.requestFields.map(_.key))
  private val edge = new TLEdgeOut(cacheOuter.clientParameters, manager, p, UnlocatableSourceInfo)

  val load = withReset(reset.asAsyncReset) { Module(new LoadUnit) }
  val pipe = withReset(reset.asAsyncReset) { Module(new LoadPipe(0)) }
  val miss = withReset(reset.asAsyncReset) { Module(new MissQueue(edge, 1)) }

  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new MemExuInput))
    val config = Option.when(HasFDI)(Input(new FDIMemoryConfig))
    val acquireReady = Input(Bool())
    val cacheRequest = Output(Valid(new DCacheWordReq))
    val permissionRequest = Output(Valid(new FDIPermissionRequest))
    val permissionResponse = Output(Valid(new FDIPermissionResponse))
    val cacheKill = Output(Bool())
    val rawMiss = Output(Valid(new MissReq))
    val rawMissReady = Output(Bool())
    val rawMissRob = Output(UInt(log2Ceil(RobSize).W))
    val primaryReady = Output(Vec(cfg.nMissEntries, Bool()))
    val secondaryReady = Output(Vec(cfg.nMissEntries, Bool()))
    val primaryAccept = Output(Vec(cfg.nMissEntries, Bool()))
    val secondaryAccept = Output(Vec(cfg.nMissEntries, Bool()))
    val allocation = Output(Bool())
    val merge = Output(Bool())
    val updateEntry = Output(UInt(log2Ceil(cfg.nMissEntries).W))
    val updateAddress = Output(UInt(PAddrBits.W))
    val entryValid = Output(Vec(cfg.nMissEntries, Bool()))
    val entryAddress = Output(Vec(cfg.nMissEntries, UInt(PAddrBits.W)))
    val acquire = Output(Valid(new TLBundleA(edge.bundle)))
    val acquireFire = Output(Bool())
    val completion = Output(Valid(new MemExuOutput))
    val queueUpdate = Output(Valid(new LqWriteBundle))
  })

  private def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  idleInputs(load.io)
  idleInputs(pipe.io)
  idleInputs(miss.io)

  load.io.ldin <> io.request
  load.io.fdiConfig.zip(io.config).foreach { case (sink, source) => sink := source }
  load.io.ldout.ready := true.B
  load.io.vecldout.ready := true.B
  load.io.lsq.ldin.ready := true.B
  load.io.lsq.ldld_nuke_query.req.ready := true.B
  load.io.lsq.stld_nuke_query.req.ready := true.B
  load.io.fast_rep_out.ready := true.B
  load.io.misalign_enq.req.ready := true.B

  // Match the accepted request's complete address and queue identity at S1.
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

  // One selected load port needs no competing-request arbiter. Neither valid nor
  // cancel is transformed between the real LoadPipe and the real MissQueue.
  miss.io.req <> pipe.io.miss_req
  pipe.io.miss_resp := miss.io.resp
  miss.io.queryMQ.head.req.valid := pipe.io.miss_req.valid
  miss.io.queryMQ.head.req.bits := pipe.io.miss_req.bits
  miss.io.mem_acquire.ready := io.acquireReady
  miss.io.mem_finish.ready := true.B
  miss.io.cmo_resp.ready := true.B
  miss.io.forward.head <> load.io.forward_mshr

  io.cacheRequest.valid := load.io.dcache.req.fire
  io.cacheRequest.bits := load.io.dcache.req.bits
  io.permissionRequest := 0.U.asTypeOf(io.permissionRequest)
  io.permissionResponse := 0.U.asTypeOf(io.permissionResponse)
  load.fdiPermission.foreach { checker =>
    io.permissionRequest.valid := observe(checker.io.req.valid) && observe(checker.io.req.ready)
    io.permissionRequest.bits := observe(checker.io.req.bits)
    io.permissionResponse.valid := observe(checker.io.resp.valid) && observe(checker.io.resp.ready)
    io.permissionResponse.bits := observe(checker.io.resp.bits)
  }
  io.cacheKill := load.io.dcache.s2_kill
  io.rawMiss.valid := miss.io.req.valid
  io.rawMiss.bits := miss.io.req.bits
  io.rawMissReady := miss.io.req.ready
  io.rawMissRob := observe(pipe.s2_req.debug_robIdx)
  io.allocation := observe(miss.miss_req_pipe_reg.alloc)
  io.merge := observe(miss.miss_req_pipe_reg.merge)
  io.updateEntry := observe(miss.miss_req_pipe_reg.mshr_id)
  io.updateAddress := observe(miss.miss_req_pipe_reg.req.addr)
  miss.entries.zipWithIndex.foreach { case (entry, index) =>
    io.primaryReady(index) := observe(entry.io.primary_ready)
    io.secondaryReady(index) := observe(entry.io.secondary_ready)
    io.primaryAccept(index) := observe(entry.primary_fire)
    io.secondaryAccept(index) := observe(entry.secondary_fire)
    io.entryValid(index) := observe(entry.req_valid)
    io.entryAddress(index) := observe(entry.req.addr)
  }
  io.acquire.valid := miss.io.mem_acquire.valid
  io.acquire.bits := miss.io.mem_acquire.bits
  io.acquireFire := miss.io.mem_acquire.fire
  io.completion.valid := load.io.ldout.fire
  io.completion.bits := load.io.ldout.bits
  io.queueUpdate.valid := load.io.lsq.ldin.fire
  io.queueUpdate.bits := load.io.lsq.ldin.bits
}
