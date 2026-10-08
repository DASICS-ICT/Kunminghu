// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._

// Native instruction mode and the existing ControlFlow mirror describe the
// current serialized context; source trust remains part of each instruction.
class FDIControlFlowSource extends Bundle {
  val sourcePrivilege = UInt(2.W)
  val sourceVirtual = Bool()
  val policy = new FDIPolicyConfig
}

// These wires expose only the existing ControlFlow mirror; target comparison
// uses all address bits and the original P06 zero-special-target rule.
class FDIControlFlowTargets extends Bundle {
  val entries = Vec(4, new FDIJumpBoundEntry)
  val mainCallEntry = UInt(64.W)
  val returnPC = UInt(64.W)
  val activeZoneReturnPC = UInt(64.W)
}
