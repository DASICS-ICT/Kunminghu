// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._

// Source privilege is independent of the access's address-translation context.
// Encoding 2 and virtualized machine mode are not supported source modes.
object FDISourcePrivilege {
  def User: UInt = 0.U(2.W)
  def Supervisor: UInt = 1.U(2.W)
  def Machine: UInt = 3.U(2.W)
}
