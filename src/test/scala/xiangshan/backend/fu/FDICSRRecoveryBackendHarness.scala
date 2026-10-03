// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import difftest.{FDIObservation, UserTimerRetire}
import org.chipsalliance.cde.config.Parameters
import utility.DelayN
import xiangshan._
import xiangshan.backend.fu.NewCSR.{FDICSRMirror, FDIMirrorClient}
import xiangshan.backend.rob.RobPtr

// The backend and FTQ are production instances. The remote mirrors model the
// existing transport, without claiming to include the ICache or LSU consumers.
class FDICSRRecoveryBackendHarness(implicit p: Parameters) extends UserTimerDeliveryHarness {
  require(HasFDI)
  val recovery = IO(Output(new Bundle {
    val owner = Vec(52, UInt(64.W))
    val effects = UInt(52.W)
    val bus = new DistributedCSRIO
    val control = Vec(13, UInt(64.W))
    val frontend = Vec(5, UInt(64.W))
    val memory = Vec(34, UInt(64.W))
    val request = Valid(new Bundle {
      val robIdx = new RobPtr
      val uid = UInt(64.W)
      val pc = UInt(64.W)
      val address = UInt(12.W)
    })
    val response = Valid(new Bundle {
      val robIdx = new RobPtr
      val flushPipe = Bool()
      val data = UInt(64.W)
      val illegal = Bool()
      val virtualFault = Bool()
    })
    val writeback = Valid(new Bundle {
      val robIdx = new RobPtr
      val flushPipe = Bool()
    })
    val robRedirect = Valid(new Redirect)
    val packets = Vec(4, new FDIObservation)
    val retired = Vec(CommitWidth, new UserTimerRetire)
  }))

  FDICSRTestAddresses.all.zipWithIndex.foreach { case (address, index) =>
    recovery.owner(index) := observe(csrMod.csrOutMap(address))
  }
  recovery.effects := VecInit(FDICSRTestAddresses.all.map(address =>
    observe(csrMod.csrRwMap(address)._1.wen))).asUInt
  recovery.bus := backend.io.mem.csrCtrl.distribute_csr
  recovery.control := observe(backendOuter.inner.intExuBlock.get.module.fdiMirror.get.io.state)
  val frontendMirror = Module(new FDICSRMirror(FDIMirrorClient.Frontend))
  val memoryMirror = Module(new FDICSRMirror(FDIMirrorClient.Memory))
  for (mirror <- Seq(frontendMirror, memoryMirror)) {
    mirror.io.distribute := DelayN(recovery.bus, 2)
    mirror.io.distribute.w.valid := RegNext(RegNext(recovery.bus.w.valid, false.B), false.B)
  }
  recovery.frontend := frontendMirror.io.state
  recovery.memory := memoryMirror.io.state
  recovery.response.valid := observe(csr.io.out.valid) && observe(csr.io.out.ready)
  recovery.response.bits.robIdx := observe(csr.io.out.bits.ctrl.robIdx)
  recovery.response.bits.flushPipe := observe(csr.io.out.bits.ctrl.flushPipe.get)
  recovery.response.bits.data := observe(csr.io.out.bits.res.data)
  recovery.response.bits.illegal := observe(csrMod.io.out.bits.EX_II)
  recovery.response.bits.virtualFault := observe(csrMod.io.out.bits.EX_VI)
  recovery.writeback.valid := observe(csrExu.io.out.valid) && observe(csrExu.io.out.ready)
  recovery.writeback.bits.robIdx := observe(csrExu.io.out.bits.robIdx)
  recovery.writeback.bits.flushPipe := observe(csrExu.io.out.bits.flushPipe.get)
  recovery.robRedirect := observe(rob.io.redirect)

  // The shared UID source observes the same allocation, retirement and backend
  // flushAfter boundary as the full reference attachment. It has no DUT outputs.
  val identities = Module(new UserTimerReferenceObserver)
  idleInputs(identities.io)
  identities.io.enabled := true.B
  identities.io.coreReset := reset.asBool
  identities.io.softwareReady := observe(csrMod.io.in.ready)
  identities.io.flush := observe(rob.io.redirect)
  rob.io.enq.req.indices.foreach { lane =>
    val source = observe(rob.io.enq.req(lane))
    identities.io.allocate(lane).valid := source.valid && source.bits.firstUop &&
      observe(rob.io.enq.canAccept) && !observe(rob.io.redirect.valid)
    identities.io.allocate(lane).bits.ptr := observe(rob.io.enq.resp(lane))
    identities.io.allocate(lane).bits.pc := source.bits.pc
    identities.io.allocate(lane).bits.instr := source.bits.instr
  }
  rob.io.commits.commitValid.indices.foreach { lane =>
    val info = observe(rob.io.commits.info(lane))
    identities.io.retire(lane).valid := observe(rob.io.commits.isCommit) &&
      observe(rob.io.commits.commitValid(lane))
    identities.io.retire(lane).bits.ptr := observe(rob.io.commits.robIdx(lane))
    identities.io.retire(lane).bits.count := info.instrSize +& CommitType.isFused(info.commitType).asUInt
    identities.io.retire(lane).bits.isTerminal := observe(rob.commitDebugUop(lane).isXSTrap)
  }
  identities.io.boundary.valid := observe(rob.exceptionHappen)
  identities.io.boundary.bits.ptr := observe(rob.io.robDeqPtr)
  identities.io.boundary.bits.isInterrupt := observe(rob.intrEnable)
  identities.io.boundary.bits.isHU := observe(rob.huAccepted)
  identities.io.architecturalTrap := observe(csrMod.hasTrap)
  val requestPtr = observe(csr.io.in.bits.ctrl.robIdx)
  val requestIds = observe(identities.ids)
  recovery.request.valid := observe(csrMod.io.in.valid) && observe(csrMod.io.in.ready)
  recovery.request.bits.robIdx := requestPtr
  recovery.request.bits.uid := requestIds(requestPtr.value)
  recovery.request.bits.pc := observe(csrMod.io.in.bits.sourcePc)
  recovery.request.bits.address := observe(csrMod.io.in.bits.addr)
  val fdiObserver = FDIReferenceObservations.connect(csr, identities)
  recovery.packets := fdiObserver.io.packets
  recovery.retired := identities.io.committed
}
