// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import utility.{DelayN, RegNextWithEnable}
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu.NewCSR._
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.Bundles._

// The memory child contains the actual StoreUnit, TLB/PMP, SQ and Sbuffer modules.
// Its externally serviced head and allocation ports now come from the real Backend.
class FDIStorePermissionBackendHarness(implicit p: Parameters) extends UserTimerDeliveryHarness {
  val storeSystem = Module(new FDIStorePermissionHarness)
  val pmp = withReset(reset.asAsyncReset) { Module(new PMP) }
  idleInputs(storeSystem.io)
  val memory = IO(new Bundle {
    val metadata = Vec(backendParams.StaCnt, new DCacheStoreIO)
    val cacheWrite = Flipped(new DCacheToSbufferIO)
    val ptw = new TlbPtwIOwithMemIdx(backendParams.StaCnt)
    val uncache = new UncacheWordIO
    val forceFlush = Input(Bool())
    val queueEmpty = Output(Bool())
    val bufferEmpty = Output(Bool())
    val flushDone = Output(Bool())
    val issue = Output(Vec(backendParams.StaCnt, Valid(new MemExuInput)))
    val dataIssue = Output(Vec(backendParams.StdCnt, Valid(new MemExuInput)))
    val completion = Output(Vec(backendParams.StaCnt, Valid(new MemExuOutput)))
    val rename = Output(Vec(RenameWidth, Valid(new DynInst)))
    val dispatchPending = Output(Vec(RenameWidth, Valid(new DynInst)))
    val dispatchReady = Output(Vec(RenameWidth, Bool()))
    val dispatchRedirect = Output(Valid(new Redirect))
    val csrSourcePc = Output(UInt(64.W))
    val publicationSq = Output(Vec(EnsbufferWidth, new SqPtr))
    val phases = Output(chiselTypeOf(storeSystem.io.phases))
    val slots = Output(chiselTypeOf(storeSystem.io.slots))
    val publication = Output(chiselTypeOf(storeSystem.io.publication))
    val writes = Output(chiselTypeOf(storeSystem.io.writes))
    val mirror = Output(Vec(34, UInt(64.W)))
    val reason = Output(UInt(64.W))
    val reasonEffect = Output(Bool())
    val supervisorTval = Output(UInt(64.W))
    val trapInstructionValid = Output(Bool())
    val sourcePrivilege = Output(UInt(2.W))
    val sourceVirtual = Output(Bool())
    val dataPrivilege = Output(UInt(2.W))
    val dataVirtual = Output(Bool())
    val machineStatus = Output(UInt(64.W))
    val queueCancel = Output(chiselTypeOf(storeSystem.io.queueCancel))
    val queueCancelEvent = Output(Bool())
    val queueDequeue = Output(chiselTypeOf(storeSystem.io.queueDequeue))
  })

  val context = DelayN(backend.io.mem.tlbCsr, 2)
  val controls = DelayN(backend.io.mem.csrCtrl, 2)
  // Observe the source and translation contexts at the memory boundary separately.
  memory.sourcePrivilege := context.priv.imode
  memory.sourceVirtual := controls.virtMode
  memory.dataPrivilege := context.priv.dmode
  memory.dataVirtual := context.priv.virt
  memory.machineStatus := observe(csrMod.mstatus.rdata)
  val memoryRedirect = RegNextWithEnable(backend.io.mem.redirect)
  storeSystem.io.tlbCsr := context
  storeSystem.io.csrCtrl := controls
  storeSystem.io.sfence := DelayN(backend.io.mem.sfence, 2)
  storeSystem.io.redirect := memoryRedirect
  pmp.io.distribute_csr := controls.distribute_csr
  storeSystem.io.pmpEnvironment.pmp := pmp.io.pmp
  storeSystem.io.pmpEnvironment.pma := pmp.io.pma
  storeSystem.io.pmpEnvironment.mode := context.priv.dmode
  storeSystem.io.pmpEnvironment.cmode := (if (HasBitmapCheck) context.mbmc.CMODE.asBool else true.B)
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
  memory.trapInstructionValid := observe(csrMod.io.trapInst.valid)
  storeSystem.io.config.foreach { config =>
    val main = Wire(new FDIMainCfgBundle)
    val library = Wire(new FDILibCfgBundle)
    main := mirror.get.word(FDIMainCfgAddress.sMainCfg)
    library := mirror.get.word(FDIBoundRegisterAddress.libCfg)
    config.policy.uEnable := main.uEnable.asBool
    config.policy.sEnable := main.sEnable.asBool
    config.policy.uCloseRead := main.uCloseRead.asBool
    config.policy.uCloseWrite := main.uCloseWrite.asBool
    config.policy.uCloseJump := main.uCloseJump.asBool
    config.policy.uCloseEcall := main.uCloseEcall.asBool
    config.policy.sCloseRead := main.sCloseRead.asBool
    config.policy.sCloseWrite := main.sCloseWrite.asBool
    config.policy.sCloseJump := main.sCloseJump.asBool
    config.policy.sCloseEcall := main.sCloseEcall.asBool
    config.sourcePrivilege := context.priv.imode
    config.sourceVirtual := controls.virtMode
    config.entries.zipWithIndex.foreach { case (entry, index) =>
      entry.boundLo := mirror.get.word(FDIBoundRegisterAddress.libBoundLo0 + 2 * index)
      entry.boundHi := mirror.get.word(FDIBoundRegisterAddress.libBoundHi0 + 2 * index)
      entry.entryValid := library.elements(s"V$index").asUInt.asBool
      entry.readAllowed := library.elements(s"R$index").asUInt.asBool
      entry.writeAllowed := library.elements(s"W$index").asUInt.asBool
    }
  }

  memory.metadata <> storeSystem.io.metadata
  memory.cacheWrite <> storeSystem.io.cacheWrite
  memory.ptw <> storeSystem.io.ptw
  memory.uncache <> storeSystem.io.uncache
  storeSystem.io.uncacheOutstanding := controls.uncache_write_outstanding_enable
  storeSystem.io.flushBuffer := memory.forceFlush || RegNext(backend.io.fenceio.sbuffer.flushSb, false.B)
  // The ordinary-store fixture admits no uncache writes, so that external queue
  // stays empty. Keep the production fence return delay and include SQ draining.
  backend.io.fenceio.sbuffer.sbIsEmpty := RegNext(storeSystem.io.flushDone, false.B)
  assert(!storeSystem.io.uncache.req.valid, "The bounded Store fixture must not request an uncached write")
  storeSystem.io.rob <> backend.io.mem.robLsqIO
  storeSystem.io.enq.lqCanAccept := true.B
  backend.io.mem.sqCanAccept := storeSystem.io.enq.canAccept
  backend.io.mem.lsqEnqIO.canAccept := storeSystem.io.enq.canAccept
  backend.io.mem.sqDeq := storeSystem.io.queueDequeue
  backend.io.mem.sqCancelCnt := storeSystem.io.queueCancel
  // These are existing child output ports, never reconstructed pointer state.
  backend.io.mem.sqDeqPtr := observe(storeSystem.queue.io.sqDeqPtr)
  // LsqWrapper exports address readiness here; its internal stIssuePtr is the enqueue tail.
  backend.io.mem.stIssuePtr := observe(storeSystem.queue.io.stAddrReadySqPtr)
  backend.io.mem.exceptionAddr.vaddr := storeSystem.io.exceptionAddress
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
    // The production memory path consumes ordinary StoreUnit results every cycle.
    when(storeSystem.io.wb(lane).valid) {
      assert(backend.io.mem.writebackSta(lane).ready, "The adopted store completion path must not backpressure")
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
      assert(backend.io.mem.writebackStd(lane).ready, "The adopted store-data completion path must not backpressure")
    }
    memory.dataIssue(lane).valid := storeSystem.io.std(lane).fire
    memory.dataIssue(lane).bits := storeSystem.io.std(lane).bits
  }
  // This fixture accepts only terminal ordinary-store results. Allowed split or
  // MMIO completion requires the separately reviewed production arbitration path.
  assert(!storeSystem.io.splitWriteback.valid && !storeSystem.io.mmioWriteback.valid,
    "The bounded ordinary-store Backend fixture cannot consume split or MMIO completion")
  // These passive leaves expose the real pre-dispatch owner and published SQ identity.
  ctrl.dispatch.io.fromRename.zipWithIndex.foreach { case (port, lane) =>
    memory.dispatchPending(lane).valid := observe(port.valid)
    memory.dispatchPending(lane).bits := observe(port.bits)
    memory.dispatchReady(lane) := observe(port.ready)
  }
  memory.dispatchRedirect := observe(ctrl.dispatch.io.redirect)
  memory.csrSourcePc := observe(csrMod.io.in.bits.sourcePc)
  for (lane <- 0 until EnsbufferWidth)
    memory.publicationSq(lane) := observe(storeSystem.queue.dataBuffer.io.deq(lane).bits.sqPtr)
  memory.queueEmpty := storeSystem.io.queueEmpty
  memory.bufferEmpty := storeSystem.io.bufferEmpty
  memory.flushDone := storeSystem.io.flushDone
  memory.phases := storeSystem.io.phases
  memory.slots := storeSystem.io.slots
  memory.publication := storeSystem.io.publication
  memory.writes := storeSystem.io.writes
  memory.queueCancel := storeSystem.io.queueCancel
  memory.queueCancelEvent := observe(storeSystem.queue.lastlastCycleRedirect)
  memory.queueDequeue := storeSystem.io.queueDequeue
  ctrl.rename.io.out.zipWithIndex.foreach { case (port, lane) =>
    memory.rename(lane).valid := observe(port.valid) && observe(port.ready)
    memory.rename(lane).bits := observe(port.bits)
  }
}
