// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Cat
import xiangshan.backend.fu.NewCSR.CSREvents.{TrapEntryHUEventSink, UretEventSink}
import xiangshan.backend.fu.UserTimer
import xiangshan.backend.fu.NewCSR.CSRBundles.{FieldInitBundle, OneFieldBundle}
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRRWField => RW}

import scala.collection.immutable.SeqMap

object UserTimerCSRAddress {
  val ustatus = 0x000
  val uie = 0x004
  val utvec = 0x005
  val uscratch = 0x040
  val uepc = 0x041
  val ucause = 0x042
  val utval = 0x043
  val uip = 0x044
  val utimer = 0x800
  val all: Seq[Int] = Seq(ustatus, uie, utvec, uscratch, uepc, ucause, utval, uip, utimer)
}

class UstatusBundle extends CSRBundle {
  val UIE = RW(0).withReset(0.U)
  val UPIE = RW(4).withReset(0.U)
}

class UepcBundle extends CSRBundle {
  // Preserve compressed instruction alignment; software cannot set bit zero.
  val PC = RW(63, 1).withReset(0.U)
}

trait UserTimerCSRs { self: NewCSR =>
  // Handler ownership survives ordinary privilege/debug traps and is not software writable.
  val userInHandler = if (HasFDI) Some(RegInit(false.B)) else None
  val userEntryEffect = WireDefault(false.B)
  val userReturnEffect = WireDefault(false.B)
  userInHandler.foreach { active =>
    when(userEntryEffect) { active := true.B }
      .elsewhen(userReturnEffect) { active := false.B }
  }
  val userTimerCSRMods: Seq[CSRModule[_]] = if (HasFDI) Seq(
    Module(new CSRModule("Ustatus", new UstatusBundle) with TrapEntryHUEventSink with UretEventSink).setAddr(UserTimerCSRAddress.ustatus),
    Module(new CSRModule("Uie", new CSRBundle {
      val UTIE = RW(4).withReset(0.U)
    })).setAddr(UserTimerCSRAddress.uie),
    Module(new CSRModule("Utvec", new CSRBundle {
      // Only direct mode is implemented; absent low bits always read as zero.
      val BASE = RW(63, 2).withReset(0.U)
    })).setAddr(UserTimerCSRAddress.utvec),
    Module(new CSRModule("Uscratch", new FieldInitBundle)).setAddr(UserTimerCSRAddress.uscratch),
    Module(new CSRModule("Uepc", new UepcBundle) with TrapEntryHUEventSink).setAddr(UserTimerCSRAddress.uepc),
    Module(new CSRModule("Ucause", new FieldInitBundle) with TrapEntryHUEventSink).setAddr(UserTimerCSRAddress.ucause),
    Module(new CSRModule("Utval", new FieldInitBundle) with TrapEntryHUEventSink).setAddr(UserTimerCSRAddress.utval)
  ) else Seq.empty

  val userTimer = if (HasFDI) Some(Module(new UserTimer)) else None
  // Read-through ports keep all countdown and pending state in the existing primitive.
  val userTimerWrite = if (HasFDI) Some(Wire(new CSRAddrWriteBundle(new OneFieldBundle))) else None
  val userPendingWrite = if (HasFDI) Some(Wire(new CSRAddrWriteBundle(new OneFieldBundle))) else None

  val userTimerCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
    userTimerCSRMods.map(mod => mod.addr -> (mod.w, mod.rdata)) ++ userTimer.toSeq.flatMap { timer =>
      Seq(
        UserTimerCSRAddress.uip -> (userPendingWrite.get, Cat(0.U(59.W), timer.io.pending, 0.U(4.W))),
        UserTimerCSRAddress.utimer -> (userTimerWrite.get, timer.io.remaining)
      )
    }
  )
  val userTimerCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    userTimerCSRMap.map { case (address, (_, data)) => address -> data }
  )

  userTimer.foreach { timer =>
    timer.io.write.valid := userTimerWrite.get.wen
    timer.io.write.bits := userTimerWrite.get.wdata
    timer.io.consume := userEntryEffect
  }
}
