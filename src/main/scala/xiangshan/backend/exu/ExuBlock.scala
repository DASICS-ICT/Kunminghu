package xiangshan.backend.exu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.fu.{CSRFileIO, FDIControlFlowSource, FDIControlFlowTargets, FenceIO}
import xiangshan.backend.fu.NewCSR.{FDIBoundRegisterAddress, FDIJumpCfgBundle, FDIMainCfgAddress, FDIMainCfgBundle, FDISpecialRegisterAddress}
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.SchdBlockParams
import xiangshan.{HasXSParameter, Redirect, XSBundle}
import utility._
import xiangshan.backend.fu.FuConfig.{AluCfg, BrhCfg}
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}

class ExuBlock(params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val exus: Seq[ExeUnit] = params.issueBlockParams.flatMap(_.exuBlockParams.map(x => LazyModule(x.genExuModule)))

  lazy val module = new ExuBlockImp(this)(p, params)
}

class ExuBlockImp(
  override val wrapper: ExuBlock
)(implicit
  p: Parameters,
  params: SchdBlockParams
) extends LazyModuleImp(wrapper) with HasCriticalErrors {
  val io = IO(new ExuBlockIO)
  val fdiMirror = Option.when(wrapper.HasFDI && io.csrio.nonEmpty) {
    val mirror = withReset(reset.asBool) {
      Module(new xiangshan.backend.fu.NewCSR.FDICSRMirror(
        xiangshan.backend.fu.NewCSR.FDIMirrorClient.ControlFlow))
    }
    mirror.io.distribute := io.csrio.get.customCtrl.distribute_csr
    mirror
  }

  private val exus = wrapper.exus.map(_.module)

  private val ins: collection.IndexedSeq[DecoupledIO[ExuInput]] = io.in.flatten
  private val outs: collection.IndexedSeq[DecoupledIO[ExuOutput]] = io.out.flatten

  (ins zip exus zip outs).foreach { case ((input, exu), output) =>
    exu.io.flush <> io.flush
    exu.io.csrio.foreach(exuio => io.csrio.get <> exuio)
    exu.io.csrin.foreach(exuio => io.csrin.get <> exuio)
    exu.io.fenceio.foreach(exuio => io.fenceio.get <> exuio)
    exu.io.frm.foreach(exuio => exuio := RegNext(io.frm.get))  // each vf exu pipe frm from csr
    exu.io.vxrm.foreach(exuio => io.vxrm.get <> exuio)
    exu.io.vlIsZero.foreach(exuio => io.vlIsZero.get := exuio)
    exu.io.vlIsVlmax.foreach(exuio => io.vlIsVlmax.get := exuio)
    exu.io.vtype.foreach(exuio => io.vtype.get := exuio)
    exu.io.in <> input
    output <> exu.io.out
    io.csrToDecode.foreach(toDecode => exu.io.csrToDecode.foreach(exuOut => toDecode := exuOut))
//    if (exu.wrapper.exuParams.fuConfigs.contains(AluCfg) || exu.wrapper.exuParams.fuConfigs.contains(BrhCfg)){
//      XSPerfAccumulate(s"${(exu.wrapper.exuParams.name)}_fire_cnt", PopCount(exu.io.in.fire))
//    }
    XSPerfAccumulate(s"${(exu.wrapper.exuParams.name)}_fire_cnt", PopCount(exu.io.in.fire))
  }
  exus.find(_.io.csrio.nonEmpty).map(_.io.csrio.get).foreach { csrio =>
    exus.map(_.io.instrAddrTransType.foreach(_ := csrio.instrAddrTransType))
    if (wrapper.HasFDI) {
      val source = Wire(new FDIControlFlowSource)
      val mainCfg = Wire(new FDIMainCfgBundle)
      mainCfg := fdiMirror.get.word(FDIMainCfgAddress.sMainCfg)
      source.sourcePrivilege := csrio.tlb.priv.imode
      source.sourceVirtual := csrio.customCtrl.virtMode
      source.policy.elements.foreach { case (name, field) =>
        field := mainCfg.elements(name).asUInt.asBool
      }
      exus.foreach(_.io.fdiSource.foreach(_ := source))

      val targets = Wire(new FDIControlFlowTargets)
      val jumpCfg = Wire(new FDIJumpCfgBundle)
      jumpCfg := fdiMirror.get.word(FDIBoundRegisterAddress.jumpCfg)
      val entryValids = Seq(jumpCfg.V0, jumpCfg.V1, jumpCfg.V2, jumpCfg.V3)
      targets.entries.zipWithIndex.foreach { case (entry, i) =>
        entry.entryValid := entryValids(i).asBool
        entry.boundLo := fdiMirror.get.word(FDIBoundRegisterAddress.jumpBoundLo0 + 2 * i)
        entry.boundHi := fdiMirror.get.word(FDIBoundRegisterAddress.jumpBoundHi0 + 2 * i)
      }
      targets.mainCallEntry := fdiMirror.get.word(FDISpecialRegisterAddress.mainCallEntry)
      targets.returnPC := fdiMirror.get.word(FDISpecialRegisterAddress.returnPC)
      targets.activeZoneReturnPC := fdiMirror.get.word(FDISpecialRegisterAddress.activeZoneReturnPC)
      exus.foreach(_.io.fdiTargets.foreach(_ := targets))

      // Serialized calls and software CSR writes share the existing owner and
      // distribution port. No extra arbitration can postpone a completed call.
      val calls = exus.flatMap(_.io.fdiCallReturnPC)
      require(calls.nonEmpty)
      val effect = Wire(Valid(UInt(wrapper.XLEN.W)))
      effect.valid := VecInit(calls.map(_.valid)).asUInt.orR
      effect.bits := Mux1H(calls.map(call => call.valid -> call.bits))
      assert(PopCount(VecInit(calls.map(_.valid))) <= 1.U,
        "Serialized FDICALL instructions must have exclusive completion")
      exus.foreach(_.io.fdiCallReturnPCIn.foreach(_ := effect))
    }
  }
  val aluFireSeq = exus.filter(_.wrapper.exuParams.fuConfigs.contains(AluCfg)).map(_.io.in.fire)
  for (i <- 0 until (aluFireSeq.size + 1)){
    XSPerfAccumulate(s"alu_fire_${i}_cnt", PopCount(aluFireSeq) === i.U)
  }
  val brhFireSeq = exus.filter(_.wrapper.exuParams.fuConfigs.contains(BrhCfg)).map(_.io.in.fire)
  for (i <- 0 until (brhFireSeq.size + 1)) {
    XSPerfAccumulate(s"brh_fire_${i}_cnt", PopCount(brhFireSeq) === i.U)
  }
  val criticalErrors = exus.filter(_.wrapper.exuParams.needCriticalErrors).flatMap(exu => exu.getCriticalErrors)
  generateCriticalErrors()
}

class ExuBlockIO(implicit p: Parameters, params: SchdBlockParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  // in(i)(j): issueblock(i), exu(j)
  val in: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(params.genExuInputCopySrcBundle)
  // out(i)(j): issueblock(i), exu(j).
  val out: MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = params.genExuOutputDecoupledBundle

  val csrio = Option.when(params.hasCSR)(new CSRFileIO)
  val csrin = Option.when(params.hasCSR)(new CSRInput)
  val csrToDecode = Option.when(params.hasCSR)(Output(new CSRToDecode))

  val fenceio = Option.when(params.hasFence)(new FenceIO)
  val frm = Option.when(params.needSrcFrm)(Input(Frm()))
  val vxrm = Option.when(params.needSrcVxrm)(Input(Vxrm()))
  val vtype = Option.when(params.writeVConfig)((Valid(new VType)))
  val vlIsZero = Option.when(params.writeVConfig)(Output(Bool()))
  val vlIsVlmax = Option.when(params.writeVConfig)(Output(Bool()))
}
