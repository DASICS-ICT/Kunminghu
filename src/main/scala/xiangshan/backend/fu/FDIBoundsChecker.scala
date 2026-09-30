// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._

class FDIBoundEntry extends Bundle {
  // Preserve exact bounds; alignment belongs to the CSR write path.
  val boundLo = UInt(64.W)
  val boundHi = UInt(64.W)
  val entryValid = Bool()
  val readAllowed = Bool()
  val writeAllowed = Bool()
}

class FDIBoundsCheckerIO extends Bundle {
  val address = Input(UInt(64.W))
  val sizeLog2 = Input(UInt(3.W))
  val operation = Input(UInt(2.W))
  val entries = Input(Vec(16, new FDIBoundEntry))
  val allow = Output(Bool())
}

class FDIBoundsChecker extends RawModule {
  val io = IO(new FDIBoundsCheckerIO)

  // Every checker sees the same complete access and only its own entry.
  private val checkers = Seq.fill(16)(Module(new FDIBoundChecker))
  for ((checker, entry) <- checkers.zip(io.entries)) {
    checker.io.address := io.address
    checker.io.sizeLog2 := io.sizeLog2
    checker.io.operation := io.operation
    checker.io.boundLo := entry.boundLo
    checker.io.boundHi := entry.boundHi
    checker.io.entryValid := entry.entryValid
    checker.io.readAllowed := entry.readAllowed
    checker.io.writeAllowed := entry.writeAllowed
  }

  // Combine complete authorizations, never partial ranges or separate R/W grants.
  io.allow := VecInit(checkers.map(_.io.allow)).asUInt.orR
}
