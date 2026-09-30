// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util.{Decoupled, MuxLookup}

class FDIPermissionRequest extends Bundle {
  val pc = UInt(64.W)
  val address = UInt(64.W)
  // The tag is opaque; every field travels with the accepted transaction.
  val tag = UInt(64.W)
  val sizeLog2 = UInt(3.W)
  val operation = UInt(2.W)
  val sourcePrivilege = UInt(2.W)
  val sourceVirtual = Bool()
  val notTrusted = Bool()
}

class FDIPermissionResponse extends Bundle {
  val request = new FDIPermissionRequest
  val outcome = UInt(2.W)
  val reason = UInt(3.W)
}

class FDIPermissionCheckerIO extends Bundle {
  val req = Flipped(Decoupled(new FDIPermissionRequest))
  val resp = Decoupled(new FDIPermissionResponse)
  // Configuration affects the result only on req.fire; it is not retained here.
  val entries = Input(Vec(16, new FDIBoundEntry))
  val config = Input(new FDIPolicyConfig)
  val flush = Input(Bool())
}

class FDIPermissionChecker extends Module with RequireSyncReset {
  val io = IO(new FDIPermissionCheckerIO)

  private val bounds = Module(new FDIBoundsChecker)
  bounds.io.address := io.req.bits.address
  bounds.io.sizeLog2 := io.req.bits.sizeLog2
  bounds.io.operation := io.req.bits.operation
  bounds.io.entries := io.entries

  private val policy = Module(new FDIPermissionPolicy)
  policy.io.rawAllow := bounds.io.allow
  policy.io.sourcePrivilege := io.req.bits.sourcePrivilege
  policy.io.sourceVirtual := io.req.bits.sourceVirtual
  policy.io.notTrusted := io.req.bits.notTrusted
  policy.io.config := io.config
  // Reserved memory operation 3 must never alias the policy's Jump encoding.
  policy.io.checkKind := MuxLookup(io.req.bits.operation, 7.U(3.W))(Seq(
    FDIAccessOperation.Read -> FDICheckKind.Read,
    FDIAccessOperation.Write -> FDICheckKind.Write,
    FDIAccessOperation.ReadWrite -> FDICheckKind.ReadWrite
  ))
  private val invalidDescriptor = io.req.bits.sizeLog2 > 4.U || io.req.bits.operation === 3.U
  private val incoming = Wire(new FDIPermissionResponse)
  incoming.request := io.req.bits
  incoming.outcome := Mux(invalidDescriptor, FDIPermissionOutcome.InvalidInput, policy.io.outcome)
  incoming.reason := Mux(invalidDescriptor, 0.U(3.W), policy.io.reason)

  // A single occupied slot owns both metadata and result until consume or cancel.
  // Unoccupied payload is unspecified and must not be consumed.
  private val occupied = RegInit(false.B)
  private val response = Reg(new FDIPermissionResponse)
  private val cancel = reset.asBool || io.flush
  io.req.ready := !cancel && (!occupied || io.resp.ready)
  io.resp.valid := !cancel && occupied
  io.resp.bits := response

  when(cancel) {
    occupied := false.B
  }.elsewhen(io.req.fire) {
    // Acceptance atomically replaces a consumed response without a bypass path.
    occupied := true.B
    response := incoming
  }.elsewhen(io.resp.fire) {
    occupied := false.B
  }
}
