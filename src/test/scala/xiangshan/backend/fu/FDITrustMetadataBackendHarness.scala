// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, ExuInput}
import xiangshan.backend.rob.RobPtr

// These observations never feed a production control or state signal.
class FDITrustBackendToken(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val pdest = UInt(PhyRegIdxWidth.W)
  val fuOpType = FuOpType()
  val vuopIdx = UInt(log2Ceil(MaxUopSize + 1).W)
  val tag = Bool()
}

class FDITrustBackendSource(implicit p: Parameters) extends XSBundle {
  val token = new FDITrustBackendToken
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val uopIdx = UInt(log2Ceil(MaxUopSize + 1).W)
  val first = Bool()
  val last = Bool()
  val eliminated = Bool()
  val psrc1 = UInt(PhyRegIdxWidth.W)
}

// CtrlFlow is the stimulus boundary: the IFU trust calculation is checked by
// the separate frontend suite. Backend, FTQ, IQ and every FU remain production.
class FDITrustMetadataBackendHarness(implicit p: Parameters) extends UserTimerDeliveryHarness {
  val queues = Seq(backendOuter.inner.intScheduler, backendOuter.inner.fpScheduler,
    backendOuter.inner.vfScheduler, backendOuter.inner.memScheduler).flatten
    .flatMap(_.issueQueue).map(_.module)
  val units = Seq(backendOuter.inner.intExuBlock, backendOuter.inner.fpExuBlock,
    backendOuter.inner.vfExuBlock).flatten.flatMap(_.exus).flatMap(_.module.funcUnits)
  val piped = units.flatMap { unit =>
    unit match {
      case pipeline: HasPipelineReg =>
        require(unit.io.in.bits.ctrlPipe.exists(_.length == pipeline.validVec.length))
        pipeline.validVec.indices.map(i => (unit, pipeline, i))
      case _ => Seq.empty
    }
  }
  val queueEnqueues = queues.flatMap(_.io.enq)
  val queueIssues = queues.flatMap(_.deqBeforeDly)
  val memoryPorts = backend.io.mem.issueUops
  val enqueueQueues = queues.zipWithIndex.flatMap { case (queue, index) => queue.io.enq.map(_ => index) }
  val issueQueues = queues.zipWithIndex.flatMap { case (queue, index) => queue.deqBeforeDly.map(_ => index) }
  // BackendMemIO appends STD ports in the same scheduler/dequeue order.
  val storeDataQueues = queues.zipWithIndex.filter(_._1.params.StdCnt > 0)
    .flatMap { case (queue, index) => queue.deqBeforeDly.map(_ => index) }
  require(storeDataQueues.size == backend.io.mem.issueStd.length)
  val memoryQueues = Seq.fill[Option[Int]](memoryPorts.size - storeDataQueues.size)(None) ++ storeDataQueues.map(Some(_))
  val addressPorts = backend.io.mem.issueSta.toSeq ++ backend.io.mem.issueHysta.toSeq
  val memoryRoles = memoryPorts.zip(memoryQueues).map { case (port, queue) =>
    if (queue.nonEmpty) "store-data" else if (addressPorts.exists(_ eq port)) "address" else "other"
  }

  val metadata = IO(new Bundle {
    val memoryReady = Input(Bool())
    val rename = Output(Vec(RenameWidth, Valid(new FDITrustBackendSource)))
    val renameHeld = Output(Vec(RenameWidth, Valid(new FDITrustBackendSource)))
    val fusion = Output(Vec(RenameWidth - 1, new Bundle {
      val valid = Bool()
      val ready = Bool()
      val pc = UInt(VAddrBits.W)
      val fused = Bool()
      val clearNext = Bool()
      val info = UInt(3.W)
      val lsrc2Valid = Bool()
      val normalPsrc1 = UInt(PhyRegIdxWidth.W)
      val secondIntRs1 = UInt(PhyRegIdxWidth.W)
      val secondIntRs2 = UInt(PhyRegIdxWidth.W)
      val actualPsrc1 = UInt(PhyRegIdxWidth.W)
    }))
    val enqueue = Output(Vec(queueEnqueues.size, Valid(new FDITrustBackendSource)))
    val issue = Output(Vec(queueIssues.size, Valid(new FDITrustBackendToken)))
    val issueFirst = Output(Vec(queueIssues.size, Bool()))
    val fu = Output(Vec(units.size, Valid(new FDITrustBackendToken)))
    val pipe = Output(Vec(piped.size, Valid(new FDITrustBackendToken)))
    val memory = Output(Vec(memoryPorts.size, Valid(new FDITrustBackendToken)))
    val memoryFire = Output(Vec(memoryPorts.size, Bool()))
  })

  def dynToken(dst: FDITrustBackendToken, src: DynInst): Unit = {
    dst.robIdx := src.robIdx
    dst.pdest := src.pdest
    dst.fuOpType := src.fuOpType
    dst.vuopIdx := src.vpu.vuopIdx
    dst.tag := src.fdiNotTrusted.getOrElse(false.B)
  }
  def source(dst: FDITrustBackendSource, src: DynInst): Unit = {
    dynToken(dst.token, src)
    dst.instr := src.instr
    dst.pc := src.pc
    dst.uopIdx := src.uopIdx
    dst.first := src.firstUop
    dst.last := src.lastUop
    dst.eliminated := src.eliminatedMove
    dst.psrc1 := src.psrc(1)
  }
  def exuToken(dst: FDITrustBackendToken, src: ExuInput): Unit = {
    dst.robIdx := src.robIdx
    dst.pdest := src.pdest
    dst.fuOpType := src.fuOpType
    dst.vuopIdx := src.vpu.map(_.vuopIdx).getOrElse(0.U)
    dst.tag := src.fdiNotTrusted.getOrElse(false.B)
  }
  def fuToken(dst: FDITrustBackendToken, src: FuncUnitCtrlInput): Unit = {
    dst.robIdx := src.robIdx
    dst.pdest := src.pdest
    dst.fuOpType := src.fuOpType
    dst.vuopIdx := src.vpu.map(_.vuopIdx).getOrElse(0.U)
    dst.tag := src.fdiNotTrusted.getOrElse(false.B)
  }

  ctrl.rename.io.out.zipWithIndex.foreach { case (port, i) =>
    val payload = observe(port.bits)
    metadata.rename(i).valid := observe(port.valid) && observe(port.ready)
    source(metadata.rename(i).bits, payload)
    // Rename compression rewrites FTQ metadata, so retain original input PC.
    metadata.rename(i).bits.pc := observe(ctrl.rename.io.in(i).bits.pc)
    metadata.renameHeld(i).valid := observe(port.valid) && !observe(port.ready)
    source(metadata.renameHeld(i).bits, payload)
    metadata.renameHeld(i).bits.pc := observe(ctrl.rename.io.in(i).bits.pc)
  }
  metadata.fusion.zipWithIndex.foreach { case (dst, i) =>
    val in = ctrl.rename.io.in(i)
    dst.valid := observe(in.valid)
    dst.ready := observe(in.ready)
    dst.pc := observe(in.bits.pc)
    dst.fused := observe(ctrl.fusionDecoder.io.out(i).valid)
    dst.clearNext := observe(ctrl.fusionDecoder.io.clear(i + 1))
    dst.info := Cat(observe(ctrl.fusionDecoder.io.info(i).rs2FromRs2),
      observe(ctrl.fusionDecoder.io.info(i).rs2FromRs1), observe(ctrl.fusionDecoder.io.info(i).rs2FromZero))
    dst.lsrc2Valid := observe(ctrl.fusionDecoder.io.out(i).bits.lsrc2.valid)
    val kind = observe(in.bits.srcType(1))
    dst.normalPsrc1 := Mux1H(kind(2, 0), Seq(observe(ctrl.rename.io.intReadPorts(i)(1)),
      observe(ctrl.rename.io.fpReadPorts(i)(1)), observe(ctrl.rename.io.vecReadPorts(i)(1))))
    // These RAT responses belong to the adjacent original instruction in the
    // same Rename cycle, even when fusion suppresses its input valid.
    dst.secondIntRs1 := observe(ctrl.rename.io.intReadPorts(i + 1)(0))
    dst.secondIntRs2 := observe(ctrl.rename.io.intReadPorts(i + 1)(1))
    dst.actualPsrc1 := observe(ctrl.rename.io.out(i).bits.psrc(1))
  }
  queueEnqueues.zip(metadata.enqueue).foreach { case (port, dst) =>
    dst.valid := observe(port.valid) && observe(port.ready)
    source(dst.bits, observe(port.bits))
  }
  queueIssues.zipWithIndex.foreach { case (port, i) =>
    metadata.issue(i).valid := observe(port.valid) && observe(port.ready)
    exuToken(metadata.issue(i).bits, observe(port.bits.common))
    metadata.issueFirst(i) := observe(port.bits.common.isFirstIssue)
  }
  units.zip(metadata.fu).foreach { case (unit, dst) =>
    dst.valid := observe(unit.io.in.valid) && observe(unit.io.in.ready)
    fuToken(dst.bits, observe(unit.io.in.bits.ctrl))
  }
  piped.zip(metadata.pipe).foreach { case ((unit, pipeline, i), dst) =>
    // ExeUnit broadcasts validPipe to sibling FUs. Only the selected FU's
    // validVec qualifies its actual control-pipeline transaction at this stage.
    dst.valid := observe(pipeline.validVec(i))
    fuToken(dst.bits, observe(unit.io.in.bits.ctrlPipe.get(i)))
  }
  memoryPorts.zipWithIndex.foreach { case (port, i) =>
    // This is external MemBlock readiness, not an override of internal ready.
    port.ready := metadata.memoryReady
    metadata.memory(i).valid := port.valid
    dynToken(metadata.memory(i).bits, port.bits.uop)
    metadata.memoryFire(i) := port.fire
  }
}

// Public FusionDecoder inputs isolate its registered qualification. This fixture
// proves capture/hold behavior; the Backend fixture proves actual Rename effects.
class FDITrustFusionHoldHarness(implicit p: Parameters) extends XSModule {
  val decoder = withReset(reset.asAsyncReset) { Module(new xiangshan.backend.decode.FusionDecoder) }
  val io = IO(new Bundle {
    val instructions = Input(Vec(2, UInt(32.W)))
    val tags = Input(Vec(2, Bool()))
    val valid = Input(Bool())
    val ready = Input(Bool())
    val disabled = Input(Bool())
    val fused = Output(Bool())
    val clearNext = Output(Bool())
    val info = Output(UInt(3.W))
    val lsrc2Valid = Output(Bool())
    val lsrc2 = Output(UInt(LogicRegsWidth.W))
  })
  decoder.io.disableFusion := io.disabled
  decoder.io.in.foreach { port => port.valid := false.B; port.bits := 0.U }
  decoder.io.inReady.foreach(_ := false.B)
  decoder.io.dec.foreach(_ := 0.U.asTypeOf(decoder.io.dec.head))
  decoder.io.fdiNotTrusted.foreach(_.foreach(_ := false.B))
  for (i <- 0 until 2) {
    decoder.io.in(i).valid := io.valid
    decoder.io.in(i).bits := io.instructions(i)
    decoder.io.fdiNotTrusted.foreach(_(i) := io.tags(i))
  }
  decoder.io.inReady(0) := io.ready
  io.fused := decoder.io.out(0).valid
  io.clearNext := decoder.io.clear(1)
  io.info := Cat(decoder.io.info(0).rs2FromRs2, decoder.io.info(0).rs2FromRs1, decoder.io.info(0).rs2FromZero)
  io.lsrc2Valid := decoder.io.out(0).bits.lsrc2.valid
  io.lsrc2 := decoder.io.out(0).bits.lsrc2.bits
}
