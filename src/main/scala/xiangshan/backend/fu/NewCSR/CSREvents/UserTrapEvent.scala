// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.AddrTransType
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.NewCSR.CSRBundles.FieldInitBundle
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, VirtMode, SatpMode}

class UserTrapEventInput extends Bundle {
  val pc = UInt(64.W)
  val target = UInt(64.W)
  val savedEnable = Bool()
  val satp = new SatpBundle
}

class TrapEntryHUEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val ustatus = Valid((new UstatusBundle).addInEvent(_.UIE, _.UPIE))
  val uepc = Valid((new UepcBundle).addInEvent(_.PC))
  val ucause = Valid((new FieldInitBundle).addInEvent(_.ALL))
  val utval = Valid((new FieldInitBundle).addInEvent(_.ALL))
  val targetPc = Valid(new TargetPCBundle)
}

class UretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val ustatus = Valid((new UstatusBundle).addInEvent(_.UIE, _.UPIE))
  val targetPc = Valid(new TargetPCBundle)
}

class TrapEntryHUEvent(implicit p: Parameters) extends Module with CSREventBase {
  val in = IO(Input(new UserTrapEventInput))
  val out = IO(new TrapEntryHUEventOutput)
  out := 0.U.asTypeOf(out)
  out.privState.valid := valid
  out.privState.bits.PRVM := PrivMode.U
  out.privState.bits.V := VirtMode.Off
  out.ustatus.valid := valid
  out.ustatus.bits.UIE := 0.U
  out.ustatus.bits.UPIE := in.savedEnable
  out.uepc.valid := valid
  out.uepc.bits.PC := in.pc(63, 1)
  out.ucause.valid := valid
  // Preserve the full-width writable UCAUSE field while supplying a fixed interrupt encoding.
  val timerCause = WireDefault("h8000000000000004".U(64.W))
  out.ucause.bits := timerCause
  out.utval.valid := valid
  out.utval.bits := 0.U
  private val translation = AddrTransType(bare = in.satp.MODE === SatpMode.Bare,
    sv39 = in.satp.MODE === SatpMode.Sv39, sv48 = in.satp.MODE === SatpMode.Sv48,
    sv39x4 = false.B, sv48x4 = false.B)
  out.targetPc.valid := valid
  out.targetPc.bits.pc := in.target
  out.targetPc.bits.raiseIPF := translation.checkPageFault(in.target)
  out.targetPc.bits.raiseIAF := translation.checkAccessFault(in.target)
  out.targetPc.bits.raiseIGPF := false.B
}

class UretEvent(implicit p: Parameters) extends Module with CSREventBase {
  val in = IO(Input(new UserTrapEventInput))
  val out = IO(new UretEventOutput)
  out := 0.U.asTypeOf(out)
  out.privState.valid := valid
  out.privState.bits.PRVM := PrivMode.U
  out.privState.bits.V := VirtMode.Off
  out.ustatus.valid := valid
  out.ustatus.bits.UIE := in.savedEnable
  out.ustatus.bits.UPIE := 1.U
  private val translation = AddrTransType(bare = in.satp.MODE === SatpMode.Bare,
    sv39 = in.satp.MODE === SatpMode.Sv39, sv48 = in.satp.MODE === SatpMode.Sv48,
    sv39x4 = false.B, sv48x4 = false.B)
  out.targetPc.valid := valid
  out.targetPc.bits.pc := in.target
  out.targetPc.bits.raiseIPF := translation.checkPageFault(in.target)
  out.targetPc.bits.raiseIAF := translation.checkAccessFault(in.target)
  out.targetPc.bits.raiseIGPF := false.B
}

trait TrapEntryHUEventSink extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val trapToHU = IO(Flipped(new TrapEntryHUEventOutput))
  addUpdateBundleInCSREnumType(trapToHU.getBundleByName(self.modName.toLowerCase()))
  reconnectReg()
}

trait UretEventSink extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val retFromU = IO(Flipped(new UretEventOutput))
  addUpdateBundleInCSREnumType(retFromU.getBundleByName(self.modName.toLowerCase()))
  reconnectReg()
}
