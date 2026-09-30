// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._

class FDIPcTrustCheckerIO extends Bundle {
  // Classify only the full instruction start PC; instruction length is not consumed here.
  val pc = Input(UInt(64.W))
  val sourcePrivilege = Input(UInt(2.W))
  val sourceVirtual = Input(Bool())
  val uEnable = Input(Bool())
  val sEnable = Input(Bool())
  // Bounds are exact unsigned [lo, hi) intervals; low bits are never discarded.
  val uBoundLo = Input(UInt(64.W))
  val uBoundHi = Input(UInt(64.W))
  val sBoundLo = Input(UInt(64.W))
  val sBoundHi = Input(UInt(64.W))
  val notTrusted = Output(Bool())
}

class FDIPcTrustChecker extends RawModule {
  val io = IO(new FDIPcTrustCheckerIO)

  private val userBound = Module(new FDIBoundChecker)
  private val supervisorBound = Module(new FDIBoundChecker)
  for (checker <- Seq(userBound, supervisorBound)) {
    checker.io.address := io.pc
    checker.io.sizeLog2 := 0.U
    checker.io.operation := FDIAccessOperation.Read
    checker.io.entryValid := true.B
    checker.io.readAllowed := true.B
    checker.io.writeAllowed := false.B
  }
  userBound.io.boundLo := io.uBoundLo
  userBound.io.boundHi := io.uBoundHi
  supervisorBound.io.boundLo := io.sBoundLo
  supervisorBound.io.boundHi := io.sBoundHi

  // Unsupported source encodings fail closed even when protection is disabled.
  io.notTrusted := true.B
  when(io.sourcePrivilege === FDISourcePrivilege.User) {
    // Enabled guests cannot inherit trust from the host's main-code bounds.
    io.notTrusted := io.uEnable && (io.sourceVirtual || !userBound.io.allow)
  }.elsewhen(io.sourcePrivilege === FDISourcePrivilege.Supervisor) {
    io.notTrusted := io.sEnable && (io.sourceVirtual || !supervisorBound.io.allow)
  }.elsewhen(io.sourcePrivilege === FDISourcePrivilege.Machine && !io.sourceVirtual) {
    io.notTrusted := false.B
  }
}
