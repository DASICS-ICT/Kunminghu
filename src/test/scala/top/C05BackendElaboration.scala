// SPDX-License-Identifier: MulanPSL-2.0

package top

import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}
import xiangshan.backend.Backend

// Elaborate the production Backend with its original IO and no test overrides.
// External inputs remain top-level ports; missing external sources are errors.
object C05BackendElaboration extends App {
  val (selected, firrtlOptions, firtoolOptions) = ArgParser.parse(args)
  require(selected(XSTileKey).size == 1, "This structural check selects one production core")
  val core = selected(XSTileKey).head
  require(core.HasVPU && core.VLEN == 128 && core.RobSize > 1 && core.RenameWidth > 1)
  implicit val parameters: Parameters = selected.alterPartial {
    case XSCoreParamsKey => core
  }
  val debug = parameters(DebugOptionsKey)
  require(debug.FPGAPlatform && !debug.EnableDifftest && !debug.AlwaysBasicDiff,
    "Backend-only structural generation requires an explicit non-DPI platform")
  Constantin.init(debug.EnableConstantin && !debug.FPGAPlatform)
  ChiselDB.init(debug.EnableChiselDB && !debug.FPGAPlatform)
  val pcConsumers = core.backendParams.allExuParams.filter(_.needPc).map(_.name)
  println(s"C05 production Backend: HasFDI=${core.HasFDI} PC consumers=${pcConsumers.mkString(",")}")
  println(s"C05 production Backend: PC ports=${core.backendParams.numPcMemReadPort} " +
    s"target ports=${core.backendParams.numTargetReadPort} VLEN=${core.VLEN}")
  val backend = DisableMonitors(p => LazyModule(new Backend(core.backendParams)(p)))(parameters)
  Generator.execute(firrtlOptions, backend.module, firtoolOptions)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
