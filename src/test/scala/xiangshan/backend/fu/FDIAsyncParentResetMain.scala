// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import xiangshan._

/** Use the production CIRCT lowering for the unchanged actual asynchronous-parent harness. */
object FDIAsyncParentResetMain extends App {
  require(args.length >= 2 && Set("true", "false").contains(args.head),
    "Expected enabled boolean followed by generator arguments")
  val enabled = args.head.toBoolean
  val base = new top.DefaultConfig
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
    case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
      EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
      EnableChiselDB = false, AlwaysBasicDB = false)
  }
  top.Generator.execute(args.tail, new FDIAsyncParentResetHarness, Array(
    "-O=release", "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"))
}
