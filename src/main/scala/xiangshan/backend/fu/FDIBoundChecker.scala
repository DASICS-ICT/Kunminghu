// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util.MuxLookup

object FDIAccessOperation {
  def Read: UInt = 0.U(2.W)
  def Write: UInt = 1.U(2.W)
  def ReadWrite: UInt = 2.U(2.W)
}

class FDIBoundCheckerIO extends Bundle {
  val address = Input(UInt(64.W))
  // Bounds are exact unsigned values; CSR alignment is the caller's responsibility.
  val boundLo = Input(UInt(64.W))
  val boundHi = Input(UInt(64.W))
  val sizeLog2 = Input(UInt(3.W))
  // Encoding 3 is reserved and must deny access.
  val operation = Input(UInt(2.W))
  val entryValid = Input(Bool())
  val readAllowed = Input(Bool())
  val writeAllowed = Input(Bool())
  val allow = Output(Bool())
}

class FDIBoundChecker extends RawModule {
  val io = IO(new FDIBoundCheckerIO)

  private val legalSize = io.sizeLog2 <= 4.U
  private val lastByteOffset = MuxLookup(io.sizeLog2, 0.U(4.W))(Seq(
    0.U -> 0.U(4.W),
    1.U -> 1.U(4.W),
    2.U -> 3.U(4.W),
    3.U -> 7.U(4.W),
    4.U -> 15.U(4.W)
  ))
  // Keep the 65th bit so a wrapped last byte cannot authorize an overflowing access.
  private val lastByte = io.address +& lastByteOffset
  private val permissionAllowed = MuxLookup(io.operation, false.B)(Seq(
    FDIAccessOperation.Read -> io.readAllowed,
    FDIAccessOperation.Write -> io.writeAllowed,
    // Read-modify-write permission must come entirely from this entry.
    FDIAccessOperation.ReadWrite -> (io.readAllowed && io.writeAllowed)
  ))

  io.allow := legalSize && io.entryValid && permissionAllowed &&
    io.boundLo < io.boundHi && io.address >= io.boundLo &&
    !lastByte(64) && lastByte(63, 0) < io.boundHi
}
