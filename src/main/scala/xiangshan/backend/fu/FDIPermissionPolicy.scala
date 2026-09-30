// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util.MuxLookup

object FDICheckKind {
  def Read: UInt = 0.U(3.W)
  def Write: UInt = 1.U(3.W)
  def ReadWrite: UInt = 2.U(3.W)
  def Jump: UInt = 3.U(3.W)
  def Ecall: UInt = 4.U(3.W)
}

// These are local policy results, not architectural exception cause numbers.
object FDIPermissionOutcome {
  def Allow: UInt = 0.U(2.W)
  def DasicsDenied: UInt = 1.U(2.W)
  def IllegalGuest: UInt = 2.U(2.W)
  def InvalidInput: UInt = 3.U(2.W)
}

class FDIPolicyConfig extends Bundle {
  val uEnable = Bool()
  val sEnable = Bool()
  val uCloseRead = Bool()
  val uCloseWrite = Bool()
  val uCloseJump = Bool()
  val uCloseEcall = Bool()
  val sCloseRead = Bool()
  val sCloseWrite = Bool()
  val sCloseJump = Bool()
  val sCloseEcall = Bool()
}

class FDIPermissionPolicyIO extends Bundle {
  val rawAllow = Input(Bool())
  // Authorization uses the instruction's origin, never its translation context.
  val sourcePrivilege = Input(UInt(2.W))
  val sourceVirtual = Input(Bool())
  val notTrusted = Input(Bool())
  val checkKind = Input(UInt(3.W))
  val config = Input(new FDIPolicyConfig)
  val outcome = Output(UInt(2.W))
  // Nonzero only for DasicsDenied: Ecall=1, Read=2, Write/ReadWrite=3, Jump=4.
  val reason = Output(UInt(3.W))
}

class FDIPermissionPolicy extends RawModule {
  val io = IO(new FDIPermissionPolicyIO)

  private val isSupervisor = io.sourcePrivilege === FDISourcePrivilege.Supervisor
  private val isMachine = io.sourcePrivilege === FDISourcePrivilege.Machine
  private val invalidSource = io.sourcePrivilege === 2.U || (isMachine && io.sourceVirtual)
  private val invalidKind = io.checkKind > FDICheckKind.Ecall
  private val enabled = Mux(isSupervisor, io.config.sEnable, io.config.uEnable)
  private val closeRead = Mux(isSupervisor, io.config.sCloseRead, io.config.uCloseRead)
  private val closeWrite = Mux(isSupervisor, io.config.sCloseWrite, io.config.uCloseWrite)
  private val closeJump = Mux(isSupervisor, io.config.sCloseJump, io.config.uCloseJump)
  private val closeEcall = Mux(isSupervisor, io.config.sCloseEcall, io.config.uCloseEcall)
  private val closed = MuxLookup(io.checkKind, false.B)(Seq(
    FDICheckKind.Read -> closeRead,
    FDICheckKind.Write -> closeWrite,
    // Read-modify-write is a store-class check; rawAllow still requires both permissions.
    FDICheckKind.ReadWrite -> closeWrite,
    FDICheckKind.Jump -> closeJump,
    FDICheckKind.Ecall -> closeEcall
  ))
  private val deniedReason = MuxLookup(io.checkKind, 0.U(3.W))(Seq(
    FDICheckKind.Read -> 2.U(3.W),
    FDICheckKind.Write -> 3.U(3.W),
    FDICheckKind.ReadWrite -> 3.U(3.W),
    FDICheckKind.Jump -> 4.U(3.W),
    FDICheckKind.Ecall -> 1.U(3.W)
  ))

  io.outcome := FDIPermissionOutcome.Allow
  io.reason := 0.U
  when(invalidSource || invalidKind) {
    io.outcome := FDIPermissionOutcome.InvalidInput
  }.elsewhen(!isMachine && enabled) {
    // An enabled guest cannot acquire host authority through trust, close, or rawAllow.
    when(io.sourceVirtual) {
      io.outcome := FDIPermissionOutcome.IllegalGuest
    }.elsewhen(io.notTrusted && !closed && (!io.rawAllow || io.checkKind === FDICheckKind.Ecall)) {
      io.outcome := FDIPermissionOutcome.DasicsDenied
      io.reason := deniedReason
    }
  }
}
