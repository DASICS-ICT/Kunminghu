// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.FtqPtr

// Selection metadata advances with valid and is immutable after ROB acceptance.
class InterruptDescriptor extends Bundle {
  val cause = UInt(8.W)
  val debug = Bool()
  val criticalDebug = Bool()
  val nmi = Bool()
  val virtualInterruptIsHvictlInject = Bool()
  val irToHS = Bool()
  val irToVS = Bool()
  val irToHU = Bool()
  val isInterrupt = Bool()
  val hvictlIID = UInt(12.W)
}

class InterruptEventIdentity(implicit p: Parameters) extends XSBundle {
  val interrupt = new InterruptDescriptor
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  val isRVC = Bool()
}

class HUEntryRequest(implicit p: Parameters) extends XSBundle {
  val event = new InterruptEventIdentity
  val pc = UInt(64.W)
}

object HUEntryOutcome {
  def enter: UInt = 0.U(2.W)
  def replay: UInt = 1.U(2.W)
  def trap: UInt = 2.U(2.W)
}

class HUEntryCompletion(implicit p: Parameters) extends XSBundle {
  val event = new InterruptEventIdentity
  val target = new TargetPCBundle
  val outcome = UInt(2.W)
}

class HUEntryCancel(implicit p: Parameters) extends XSBundle {
  val event = new InterruptEventIdentity
  val externalRedirect = Bool()
}

// Reservation freezes qualification before the precise PC crosses the execution pipe.
// Completion transfers a terminal result; release follows its frontend consumption.
class HUEntryPort(implicit p: Parameters) extends XSBundle {
  val reserve = Flipped(Decoupled(new InterruptEventIdentity))
  val request = Flipped(Decoupled(new HUEntryRequest))
  val cancel = Input(Valid(new HUEntryCancel))
  val completion = Decoupled(new HUEntryCompletion)
  // Registered terminal ownership also covers debug effects before target readiness.
  val effectLocked = Output(Bool())
  val canceled = Output(Bool())
  val release = Input(Bool())
  val satpMode = Output(UInt(4.W))
}

// CSR-facing orientation. The ROB owns acceptance; Control owns target delivery.
class UserTimerDeliveryIO(implicit p: Parameters) extends XSBundle {
  val candidate = Output(Valid(new InterruptDescriptor))
  val candidateKill = Output(Bool())
  val accepted = Input(Valid(new InterruptEventIdentity))
  val entry = new HUEntryPort
}
