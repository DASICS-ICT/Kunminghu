// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{FtqPtr, RVCExpander}

object FDIFullControlAddresses {
  val owner: Seq[Int] = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e1, 0x9e2, 0x9e3, 0x880) ++
    (0x890 to 0x8af) ++ (0x8b0 to 0x8b3) ++ (0x8c0 to 0x8c8)
  val mirror: Seq[Int] = Seq(0xbc4, 0x8c8) ++ (0x8c0 to 0x8c7) ++ Seq(0x8b0, 0x8b1, 0x8b2)
}

class FDIFullControlSource(implicit p: Parameters) extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val instr = UInt(32.W)
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val operation = FuOpType()
  val rfWen = Bool()
  val tag = Bool()
  val waitForward = Bool()
  val blockBackward = Bool()
}

class FDIFullControlExecution(implicit p: Parameters) extends XSBundle {
  val inFire = Bool()
  val outFire = Bool()
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val operation = FuOpType()
  val tag = Bool()
  val rfWen = Bool()
  val data = UInt(64.W)
  val exception = UInt(ExceptionVec.ExceptionVecSize.W)
  val tval = UInt(64.W)
  val reason = UInt(3.W)
  val redirect = Valid(new Redirect)
  val call = Valid(UInt(64.W))
}

// The frontend service supplies raw instructions and independently derived tags.
// Compressed words pass through the same expander used by IFU. All execution,
// register-file writes, CSR ownership and retirement remain production logic.
class FDIFullControlHarness(implicit p: Parameters) extends UserTimerDeliveryHarness {
  val controlUnits = backendOuter.inner.intExuBlock.get.exus.flatMap(_.module.funcUnits)
    .filter(unit => unit.cfg.isJmp || unit.cfg.isBrh)
  val controlKinds = controlUnits.map(unit => if (unit.cfg.isJmp) "jump" else "branch")
  val intWrites = backendOuter.inner.dataPath.module.io.fromIntWb
  val control = IO(Output(new Bundle {
    val renamed = Vec(RenameWidth, Valid(new FDIFullControlSource))
    val execution = Vec(controlUnits.size, new FDIFullControlExecution)
    val rf = Vec(intWrites.size, new Bundle {
      val valid = Bool()
      val pdest = UInt(PhyRegIdxWidth.W)
      val data = UInt(64.W)
    })
    val expanded = Vec(DecodeWidth, UInt(32.W))
    val owner = Vec(52, UInt(64.W))
    val mirror = Vec(13, UInt(64.W))
    val call = Valid(UInt(64.W))
    val distribution = new DistributedCSRIO
    val waitForward = Bool()
    val blockBackward = Bool()
    val robEmpty = Bool()
    val redirect = Valid(new Redirect)
    val stval = UInt(64.W)
    val csrWriteRequest = Bool()
    val csrResult = Valid(new Bundle {
      val robIdx = new RobPtr
      val pdest = UInt(PhyRegIdxWidth.W)
      val exception = UInt(ExceptionVec.ExceptionVecSize.W)
    })
    val exceptionIngress = Vec(rob.exceptionGen.io.wb.size, Valid(new Bundle {
      val robIdx = new RobPtr
      val exception = UInt(ExceptionVec.ExceptionVecSize.W)
      val eligible = Bool()
      val raw = Valid(UInt(32.W))
    }))
  }))

  (io.instruction +: io.instructionTail.toSeq).zipWithIndex.foreach { case (source, i) =>
    val expander = Module(new RVCExpander)
    expander.io.in := source.bits.instr
    expander.io.fsIsOff := true.B
    val expanded = Mux(expander.io.ill, source.bits.instr, expander.io.out.bits)
    backend.io.frontend.cfVec(i).bits.instr := Mux(source.bits.pd.isRVC, expanded, source.bits.instr)
    // This service boundary retains the same original halfword that a real
    // IFU supplies alongside expansion, under the unchanged lane handshake.
    backend.io.frontend.cfVec(i).bits.fdiRawRVC.foreach(_ := source.bits.instr(15, 0))
    control.expanded(i) := backend.io.frontend.cfVec(i).bits.instr
  }
  ctrl.rename.io.out.zipWithIndex.foreach { case (port, i) =>
    val payload = observe(port.bits)
    val dst = control.renamed(i)
    dst.valid := observe(port.valid) && observe(port.ready)
    dst.bits.pc := observe(ctrl.rename.io.in(i).bits.pc)
    dst.bits.instr := payload.instr
    dst.bits.robIdx := payload.robIdx
    dst.bits.ftqIdx := payload.ftqPtr
    dst.bits.ftqOffset := payload.ftqOffset
    dst.bits.pdest := payload.pdest
    dst.bits.operation := payload.fuOpType
    dst.bits.rfWen := payload.rfWen
    dst.bits.tag := payload.fdiNotTrusted.getOrElse(false.B)
    dst.bits.waitForward := payload.waitForward
    dst.bits.blockBackward := payload.blockBackward
  }
  controlUnits.zip(control.execution).foreach { case (unit, dst) =>
    val in = observe(unit.io.in.bits)
    val out = observe(unit.io.out.bits)
    dst.inFire := observe(unit.io.in.valid) && observe(unit.io.in.ready)
    dst.outFire := observe(unit.io.out.valid) && observe(unit.io.out.ready)
    dst.robIdx := in.ctrl.robIdx
    dst.ftqIdx := in.ctrl.ftqIdx.get
    dst.ftqOffset := in.ctrl.ftqOffset.get
    dst.pdest := in.ctrl.pdest
    dst.operation := in.ctrl.fuOpType
    dst.tag := in.ctrl.fdiNotTrusted.getOrElse(false.B)
    dst.rfWen := out.ctrl.rfWen.getOrElse(false.B)
    dst.data := out.res.data
    dst.exception := out.ctrl.exceptionVec.get.asUInt
    dst.tval := out.ctrl.fdiException.map(_.tval).getOrElse(0.U)
    dst.reason := out.ctrl.fdiException.map(_.reason).getOrElse(0.U)
    dst.redirect := out.res.redirect.get
    dst.call := unit.io.fdiCallReturnPC.map(observe(_)).getOrElse(0.U.asTypeOf(dst.call))
  }
  intWrites.zip(control.rf).foreach { case (port, dst) =>
    dst.valid := observe(port.wen) && observe(port.intWen)
    dst.pdest := observe(port.addr)
    dst.data := observe(port.data)
  }
  control.owner := VecInit(FDIFullControlAddresses.owner.map(address =>
    if (HasFDI) observe(csrMod.csrOutMap(address)) else 0.U(64.W)))
  control.mirror := backendOuter.inner.intExuBlock.get.module.fdiMirror
    .map(mirror => observe(mirror.io.state)).getOrElse(0.U.asTypeOf(control.mirror))
  control.call := csrMod.io.fdiCallReturnPC.map(observe(_)).getOrElse(0.U.asTypeOf(control.call))
  control.distribution := backend.io.mem.csrCtrl.distribute_csr
  control.waitForward := observe(rob.hasWaitForward)
  control.blockBackward := observe(rob.hasBlockBackward)
  control.robEmpty := observe(rob.isEmpty)
  control.redirect := observe(ctrl.io.redirect)
  control.stval := observe(csrMod.stval.rdata)
  control.csrWriteRequest := observe(csrMod.io.in.valid) && observe(csrMod.io.in.ready) &&
    observe(csrMod.io.in.bits.wen)
  control.csrResult.valid := observe(csr.io.out.valid) && observe(csr.io.out.ready)
  control.csrResult.bits.robIdx := observe(csr.io.out.bits.ctrl.robIdx)
  control.csrResult.bits.pdest := observe(csr.io.out.bits.ctrl.pdest)
  control.csrResult.bits.exception := observe(csr.io.out.bits.ctrl.exceptionVec.get).asUInt
  rob.exceptionGen.io.wb.zip(control.exceptionIngress).foreach { case (port, dst) =>
    dst.valid := observe(port.valid)
    dst.bits.robIdx := observe(port.bits.robIdx)
    dst.bits.exception := observe(port.bits.exceptionVec).asUInt
    dst.bits.raw := port.bits.fdiTrapInst.map(observe(_)).getOrElse(0.U.asTypeOf(dst.bits.raw))
    // Tap the existing valid supplied to the native first age comparison.
    // The driver separately requires a nonempty II mask before counting a race.
    val group = rob.exceptionGen.writebacks.indexWhere(_.exists(_ eq port))
    require(group >= 0)
    val slot = rob.exceptionGen.writebacks(group).indexWhere(_ eq port)
    dst.bits.eligible := observe(rob.exceptionGen.in_wb_valids(group)(slot))
  }
}
