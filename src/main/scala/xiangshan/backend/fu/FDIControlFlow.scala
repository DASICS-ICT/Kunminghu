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
