package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import fudian.SignExt
import xiangshan.{ExceptionNO, JumpOpType, RedirectLevel}
import xiangshan.backend.fu.{FDISourcePrivilege, FuConfig, FuncUnit, JumpDataModule, PipedFuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData


class JumpUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val jumpDataModule = Module(new JumpDataModule)

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  // associated with AddrData's position of JmpCfg.srcData
  private val src = io.in.bits.data.src(0)
  private val pc = SignExt(io.in.bits.data.pc.get, cfg.destDataBits)
  private val imm = io.in.bits.data.imm
  private val func = io.in.bits.ctrl.fuOpType
  private val isRVC = io.in.bits.ctrl.preDecode.get.isRVC
  private val live = if (HasFDI) io.in.valid && !flushed && !reset.asBool else io.in.valid
  private val isFdiCall = func === JumpOpType.fdicallJ || func === JumpOpType.fdicallJR
  private val illegalCall = if (HasFDI) {
    val source = io.fdiSource.get
    val hostUser = source.sourcePrivilege === FDISourcePrivilege.User
    val hostSupervisor = source.sourcePrivilege === FDISourcePrivilege.Supervisor
    val enabled = Mux(hostSupervisor, source.policy.sEnable, source.policy.uEnable)
    isFdiCall && !(!source.sourceVirtual && (hostUser || hostSupervisor) &&
      enabled && !io.in.bits.ctrl.fdiNotTrusted.get)
  } else false.B

  jumpDataModule.io.src := src
  jumpDataModule.io.pc := pc
  jumpDataModule.io.imm := imm
  jumpDataModule.io.nextPcOffset := io.in.bits.data.nextPcOffset.get
  jumpDataModule.io.func := func
  jumpDataModule.io.isRVC := isRVC

  val jmpTarget = io.in.bits.ctrl.predictInfo.get.target
  val predTaken = io.in.bits.ctrl.predictInfo.get.taken

  val redirect = io.out.bits.res.redirect.get.bits
  val redirectValid = io.out.bits.res.redirect.get.valid
  redirectValid := live && !jumpDataModule.io.isAuipc && !illegalCall
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.ctrl.robIdx
  redirect.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  redirect.fullTarget := jumpDataModule.io.target
  redirect.cfiUpdate.predTaken := true.B
  redirect.cfiUpdate.taken := true.B
  redirect.cfiUpdate.target := jumpDataModule.io.target
  redirect.cfiUpdate.pc := io.in.bits.data.pc.get
  redirect.cfiUpdate.isMisPred := jumpDataModule.io.target(VAddrData().dataWidth - 1, 0) =/= jmpTarget || !predTaken
  redirect.cfiUpdate.backendIAF := io.instrAddrTransType.get.checkAccessFault(jumpDataModule.io.target)
  redirect.cfiUpdate.backendIPF := io.instrAddrTransType.get.checkPageFault(jumpDataModule.io.target)
  redirect.cfiUpdate.backendIGPF := io.instrAddrTransType.get.checkGuestPageFault(jumpDataModule.io.target)
//  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  io.in.ready := io.out.ready
  io.out.valid := live
  io.out.bits.res.data := Mux(illegalCall, 0.U, jumpDataModule.io.result)
  connect0LatencyCtrlSingal
  if (HasFDI) {
    io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := illegalCall
    io.out.bits.ctrl.rfWen.get := io.in.bits.ctrl.rfWen.get && !illegalCall
    // rd=x0 removes only the link destination. The implicit ReturnPC effect
    // still belongs to this same successful, non-canceled output handshake.
    io.fdiCallReturnPC.get.valid := io.out.fire && isFdiCall && !illegalCall
    io.fdiCallReturnPC.get.bits := jumpDataModule.io.result
  }
}
