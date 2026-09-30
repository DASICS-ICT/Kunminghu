// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._

class FDIJumpBoundEntry extends Bundle {
  // Bounds retain all address bits; normalization belongs to the caller.
  val boundLo = UInt(64.W)
  val boundHi = UInt(64.W)
  val entryValid = Bool()
}

class FDIJumpTargetCheckerIO extends Bundle {
  val target = Input(UInt(64.W))
  val entries = Input(Vec(4, new FDIJumpBoundEntry))
  // Zero disables each special target independently.
  val mainCallEntry = Input(UInt(64.W))
  val returnPC = Input(UInt(64.W))
  val activeZoneReturnPC = Input(UInt(64.W))
  val allow = Output(Bool())
}

class FDIJumpTargetChecker extends RawModule {
  val io = IO(new FDIJumpTargetCheckerIO)

  private val rangeAllows = io.entries.map { entry =>
    val checker = Module(new FDIBoundChecker)
    checker.io.address := io.target
    checker.io.boundLo := entry.boundLo
    checker.io.boundHi := entry.boundHi
    // Membership concerns the target byte, not the length of its instruction.
    checker.io.sizeLog2 := 0.U
    checker.io.operation := FDIAccessOperation.Read
    checker.io.entryValid := entry.entryValid
    checker.io.readAllowed := true.B
    checker.io.writeAllowed := false.B
    checker.io.allow
  }
  private val specialAllows = Seq(io.mainCallEntry, io.returnPC, io.activeZoneReturnPC).map { target =>
    target =/= 0.U && io.target === target
  }

  io.allow := (rangeAllows ++ specialAllows).reduce(_ || _)
}
