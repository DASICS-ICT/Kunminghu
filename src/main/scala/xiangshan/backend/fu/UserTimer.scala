// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util.Valid

class UserTimer extends Module {
  val io = IO(new Bundle {
    // The caller supplies the qualified counting cycles, independently of interrupt delivery masks.
    val tickEnable = Input(Bool())
    // Valid denotes an actual permitted write, not a CSR read or an uncommitted request.
    val write = Flipped(Valid(UInt(64.W)))
    val consume = Input(Bool())
    val remaining = Output(UInt(64.W))
    val pending = Output(Bool())
  })

  private val remaining = RegInit(0.U(64.W))
  // Expiration remains pending until a write or explicit consumption, even when ticking is disabled.
  private val pending = RegInit(false.B)

  // RegInit gives reset priority over writes, consumption, and qualified ticks.
  when(io.write.valid) {
    remaining := io.write.bits
    pending := false.B
  }.elsewhen(io.consume) {
    // Consumption holds the count and excludes a tick in the same cycle.
    pending := false.B
  }.elsewhen(io.tickEnable && remaining =/= 0.U) {
    remaining := remaining - 1.U
    when(remaining === 1.U) {
      pending := true.B
    }
  }

  io.remaining := remaining
  io.pending := pending
}
