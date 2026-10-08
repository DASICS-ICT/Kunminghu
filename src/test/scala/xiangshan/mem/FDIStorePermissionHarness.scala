// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.FDIExceptionRecord
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.fu.{FDIPermissionRequest, FDIPermissionResponse, PMPChecker, PMPCheckerEnv}
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem.Bundles._

// All permission, queue and publication decisions remain in production modules.
// External services own only ROB inputs, cache replies, PTW and PMP/PMA entries.
class FDIStorePermissionHarness(implicit p: Parameters) extends XSModule {
  require(backendParams.StaCnt == 2 && backendParams.StdCnt == 2 && backendParams.HyuCnt == 0)
  require(EnsbufferWidth == 2 && StorePipelineWidth == 2 && HasVPU && VLEN == 128)
  val stores = Seq.fill(backendParams.StaCnt)(withReset(reset.asAsyncReset) { Module(new StoreUnit) })
  private val stdParameters = backendParams.memSchdParams.get.issueBlockParams
    .find(_.StdCnt != 0).get.exuBlockParams.head
  val dataUnits = Seq.fill(backendParams.StdCnt)(withReset(reset.asAsyncReset) {
    Module(new MemExeUnit(stdParameters))
  })
  val translation = withReset(reset.asAsyncReset) {
    Module(new TLBNonBlock(backendParams.StaCnt, 1, sttlbParams))
  }
  val protection = Seq.fill(backendParams.StaCnt)(withReset(reset.asAsyncReset) {
    Module(new PMPChecker(4, leaveHitMux = true))
  })
  val queue = withReset(reset.asAsyncReset) { Module(new StoreQueue) }
  val buffer = withReset(reset.asAsyncReset) { Module(new Sbuffer) }
  val split = withReset(reset.asAsyncReset) { Module(new StoreMisalignBuffer) }

  val io = IO(new Bundle {
    val enq = new SqEnqIO
    val sta = Vec(stores.size, Flipped(Decoupled(new MemExuInput)))
    val std = Vec(dataUnits.size, Flipped(Decoupled(new MemExuInput)))
    val wb = Output(Vec(stores.size, Valid(new MemExuOutput)))
    val dataWb = Output(Vec(dataUnits.size, Valid(new MemExuOutput)))
    val feedback = Output(Vec(stores.size, Valid(new RSFeedback)))
    val config = Option.when(HasFDI)(Input(new FDIMemoryConfig))
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val fromCsrTrigger = Input(chiselTypeOf(stores.head.io.fromCsrTrigger))
    val tlbCsr = Input(new TlbCsrBundle)
    val sfence = Input(new SfenceBundle)
    val redirect = Flipped(Valid(new Redirect))
    val rob = Flipped(new RobLsqIO)
    val pmpEnvironment = Input(new PMPCheckerEnv)
    val ptw = new TlbPtwIOwithMemIdx(stores.size)
    val metadata = Vec(stores.size, new DCacheStoreIO)
    val cacheWrite = Flipped(new DCacheToSbufferIO)
    val uncache = new UncacheWordIO
    val uncacheOutstanding = Input(Bool())
    val flushBuffer = Input(Bool())
    val bufferEmpty = Output(Bool())
    val flushDone = Output(Bool())
    val queueEmpty = Output(Bool())
    val queueDequeue = Output(chiselTypeOf(queue.io.sqDeq))
    val queueCancel = Output(chiselTypeOf(queue.io.sqCancelCnt))
    val splitRequest = Output(Valid(new LsPipelineBundle))
    val splitRequestFire = Output(Bool())
    val splitWriteback = Output(Valid(new MemExuOutput))
    val mmioWriteback = Output(Valid(new MemExuOutput))
    val exceptionAddress = Output(UInt(XLEN.W))
    val phases = Output(Vec(stores.size, new Bundle {
      val s0 = Bool()
      val s1 = Bool()
      val s2 = Bool()
      val address = UInt(XLEN.W)
      val source = new RobPtr
      val pmpFault = Bool()
      val tlbResponse = Valid(new TlbResp(1))
      val primary = Valid(new LsPipelineBundle)
      val supplement = new LsPipelineBundle
      val permissionRequest = Valid(new FDIPermissionRequest)
      val permissionResponse = Valid(new FDIPermissionResponse)
      val permissionConsumed = Bool()
    }))
    val slots = Output(Vec(StoreQueueSize, new Bundle {
      val allocated = Bool()
      val addressReady = Bool()
      val dataReady = Bool()
      val committed = Bool()
      val waitS2 = Bool()
      val hasException = Bool()
      val nc = Bool()
      val mmio = Bool()
      val pending = Bool()
      val rob = new RobPtr
      val sq = new SqPtr
      val exceptions = ExceptionVec()
      val fdiException = Option.when(HasFDI)(new FDIExceptionRecord)
    }))
    val publication = Output(Vec(EnsbufferWidth, new Bundle {
      val valid = Bool()
      val ready = Bool()
      val fire = Bool()
      val bits = new DCacheWordReqWithVaddrAndPfFlag
      val sq = new SqPtr
    }))
    val writes = Output(Vec(EnsbufferWidth, Valid(new DataWriteReq)))
    val bufferState = Output(chiselTypeOf(buffer.stateVec))
    val bufferPhysicalTags = Output(chiselTypeOf(buffer.ptag))
    val bufferVirtualTags = Output(chiselTypeOf(buffer.vtag))
    val bufferData = Output(chiselTypeOf(buffer.data))
    val bufferMask = Output(chiselTypeOf(buffer.mask))
  })

  private def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  (stores.map(_.io) ++ dataUnits.map(_.io) ++ protection.map(_.io)).foreach(idleInputs)
  Seq(translation.io, queue.io, buffer.io, split.io).foreach(idleInputs)

  queue.io.enq <> io.enq
  queue.io.brqRedirect := io.redirect
  queue.io.rob <> io.rob
  queue.io.uncache <> io.uncache
  queue.io.uncacheOutstanding := io.uncacheOutstanding
  queue.io.hartId := 0.U
  queue.io.exceptionAddr.isStore := true.B
  queue.io.mmioStout.ready := true.B
  queue.io.cboZeroStout.ready := true.B
  queue.io.vecmmioStout.ready := true.B
  queue.io.maControl <> split.io.sqControl
  io.queueEmpty := queue.io.sqEmpty
  io.queueDequeue := queue.io.sqDeq
  io.queueCancel := queue.io.sqCancelCnt
  io.exceptionAddress := queue.io.exceptionAddr.vaddr
  io.mmioWriteback.valid := queue.io.mmioStout.fire
  io.mmioWriteback.bits := queue.io.mmioStout.bits

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

  split.io.redirect := io.redirect
  split.io.rob.lcommit := io.rob.lcommit
  split.io.rob.scommit := io.rob.scommit
  split.io.rob.pendingMMIOld := io.rob.pendingMMIOld
  split.io.rob.pendingld := io.rob.pendingld
  split.io.rob.pendingst := io.rob.pendingst
  split.io.rob.pendingVst := io.rob.pendingVst
  split.io.rob.commit := io.rob.commit
  split.io.rob.pendingPtr := io.rob.pendingPtr
  split.io.rob.pendingPtrNext := io.rob.pendingPtrNext
  split.io.storeOutValid := stores.head.io.stout.valid
  split.io.storeVecOutValid := stores.head.io.vecstout.valid
  split.io.writeBack.ready := !stores.head.io.stout.valid && !stores.head.io.vecstout.valid &&
    !queue.io.mmioStout.valid && !queue.io.cboZeroStout.valid
  io.splitRequest.valid := split.io.splitStoreReq.valid
  io.splitRequest.bits := split.io.splitStoreReq.bits
  io.splitRequestFire := split.io.splitStoreReq.fire
  io.splitWriteback.valid := split.io.writeBack.valid
  io.splitWriteback.bits := split.io.writeBack.bits

  stores.zipWithIndex.foreach { case (store, lane) =>
    store.io.stin <> io.sta(lane)
    store.io.redirect := io.redirect
    store.io.csrCtrl := io.csrCtrl
    store.io.fromCsrTrigger := io.fromCsrTrigger
    store.io.fdiConfig.foreach(_ := io.config.get)
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
    split.io.enq(lane) <> store.io.misalign_enq
    if (lane == 0) {
      store.io.misalign_stin <> split.io.splitStoreReq
      split.io.splitStoreResp := store.io.misalign_stout
    }
    io.wb(lane).valid := store.io.stout.fire
    io.wb(lane).bits := store.io.stout.bits
    io.feedback(lane) := store.io.feedback_slow
    val phase = io.phases(lane)
    phase.s0 := observe(store.s0_fire)
    phase.s1 := observe(store.s1_fire)
    phase.s2 := observe(store.s2_fire)
    phase.address := observe(store.s1_out.fullva)
    phase.source := observe(store.s1_out.uop.robIdx)
    phase.pmpFault := store.io.pmp.st
    phase.tlbResponse.valid := store.io.tlb.resp.valid
    phase.tlbResponse.bits := store.io.tlb.resp.bits
    phase.primary := store.io.lsq
    phase.supplement := store.io.lsq_replenish
    phase.permissionRequest := 0.U.asTypeOf(phase.permissionRequest)
    phase.permissionResponse := 0.U.asTypeOf(phase.permissionResponse)
    phase.permissionConsumed := false.B
    store.fdiPermission.foreach { checker =>
      phase.permissionRequest.valid := observe(checker.io.req.valid) && observe(checker.io.req.ready)
      phase.permissionRequest.bits := observe(checker.io.req.bits)
      phase.permissionResponse.valid := observe(checker.io.resp.valid)
      phase.permissionResponse.bits := observe(checker.io.resp.bits)
      phase.permissionConsumed := observe(checker.io.resp.valid) && observe(checker.io.resp.ready)
    }
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
    val slot = io.slots(index)
    slot.allocated := observe(queue.allocated(index))
    slot.addressReady := observe(queue.addrvalid(index))
    slot.dataReady := observe(queue.datavalid(index))
    slot.committed := observe(queue.committed(index))
    slot.waitS2 := observe(queue.waitStoreS2(index))
    slot.hasException := observe(queue.hasException(index))
    slot.nc := observe(queue.nc(index))
    slot.mmio := observe(queue.mmio(index))
    slot.pending := observe(queue.pending(index))
    slot.rob := observe(queue.uop(index).robIdx)
    slot.sq := observe(queue.uop(index).sqIdx)
    slot.exceptions := observe(queue.uop(index).exceptionVec)
    slot.fdiException.foreach(_ := observe(queue.uop(index).fdiException.get))
  }
  for (lane <- 0 until EnsbufferWidth) {
    val port = buffer.io.in(lane)
    io.publication(lane).valid := port.valid
    io.publication(lane).ready := port.ready
    io.publication(lane).fire := port.fire
    io.publication(lane).bits := port.bits
    io.publication(lane).sq := observe(queue.dataBuffer.io.deq(lane).bits.sqPtr)
    io.writes(lane) := observe(buffer.writeReq(lane))
  }
  io.bufferState := observe(buffer.stateVec)
  io.bufferPhysicalTags := observe(buffer.ptag)
  io.bufferVirtualTags := observe(buffer.vtag)
  io.bufferData := observe(buffer.data)
  io.bufferMask := observe(buffer.mask)
}
