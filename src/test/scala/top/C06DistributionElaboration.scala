// SPDX-License-Identifier: MulanPSL-2.0

package top

import chisel3._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{DebugOptionsKey, XSTileKey}

class C06ObservedSimTop(implicit p: Parameters) extends SimTop {
  override protected def addDifftestObservations(): Unit = {
    val core = l_soc.core_with_l2.head.core
    if (core.coreParams.HasFDI) {
      val frontend = core.frontend.inner.module.fdiMirror.get
      val control = core.backend.inner.intExuBlock.get.module.fdiMirror.get
      val memory = core.memBlock.inner.module.fdiMirror.get
      // Read-only test outputs preserve actual block-local state and its real
      // input routes without forcing any production request or register value.
      for ((name, mirror) <- Seq("frontend" -> frontend, "control" -> control, "memory" -> memory)) {
        val state = IO(Output(Vec(mirror.client.addresses.size, UInt(64.W)))).suggestName(s"fdi_${name}_state")
        state.zip(mirror.io.state).foreach { case (sink, source) => sink := observe(source) }
        val input = IO(Output(chiselTypeOf(mirror.io.distribute))).suggestName(s"fdi_${name}_input")
        input.w.valid := observe(mirror.io.distribute.w.valid)
        input.w.bits.addr := observe(mirror.io.distribute.w.bits.addr)
        input.w.bits.data := observe(mirror.io.distribute.w.bits.data)
      }
    }
  }
}

object C06DistributionElaboration extends App {
  val (config, firrtlOptions, firtoolOptions) = ArgParser.parse(args)
  require(config(XSTileKey).size == 1)
  require(config(XSTileKey).head.HasVPU && config(XSTileKey).head.VLEN == 128)
  val debug = config(DebugOptionsKey)
  require(debug.FPGAPlatform && !debug.EnableDifftest && !debug.AlwaysBasicDiff,
    "The structural observer does not open a runtime comparison mode")
  Constantin.init(debug.EnableConstantin && !debug.FPGAPlatform)
  ChiselDB.init(debug.EnableChiselDB && !debug.FPGAPlatform)
  Generator.execute(firrtlOptions, DisableMonitors(p => new C06ObservedSimTop()(p))(config), firtoolOptions)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
