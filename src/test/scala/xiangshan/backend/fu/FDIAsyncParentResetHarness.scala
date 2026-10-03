// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** The explicit input drives the same asynchronous parent reset as full SimTop. */
class FDIAsyncParentResetHarness(implicit p: Parameters) extends Module {
  val coreReset = IO(Input(Bool()))
  val csr = withReset(coreReset.asAsyncReset) { Module(new FDICSRIntegrationHarness) }
  val io = IO(chiselTypeOf(csr.io))
  io <> csr.io
}
