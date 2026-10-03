// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan.{ExceptionNO, ExceptionVec}

// Validity and origin are carried by the same transaction's exception vector.
// The address remains XLEN-wide even when ordinary pipeline VAs are narrower.
class FDIExceptionRecord extends Bundle {
  val tval = UInt(64.W)
  val reason = UInt(3.W)
}

object FDIExceptionRecord {
  def pending(vector: Vec[Bool]): Bool =
    vector(ExceptionNO.dasicsU) || vector(ExceptionNO.dasicsS)

  // A denied host access produces exactly one origin-specific exception. Guest
  // policy errors are ordinary illegal instructions and are handled upstream.
  def exceptionVector(denied: Bool, privilege: UInt, virtual: Bool): Vec[Bool] = {
    val result = WireInit(ExceptionVec(false.B))
    result(ExceptionNO.dasicsU) := denied && !virtual && privilege === 0.U
    result(ExceptionNO.dasicsS) := denied && !virtual && privilege === 1.U
    result
  }

  def check(valid: Bool, vector: Vec[Bool], record: FDIExceptionRecord): Unit = {
    when(valid) {
      assert(!(vector(ExceptionNO.dasicsU) && vector(ExceptionNO.dasicsS)),
        "A DASICS fault must have exactly one host origin")
      when(pending(vector)) {
        assert(record.reason >= 1.U && record.reason <= 4.U,
          "A DASICS fault must carry a hardware-defined reason")
        when(record.reason === 1.U) {
          assert(record.tval === 0.U, "A restricted ECALL has zero TVAL")
        }
      }
    }
  }
}
