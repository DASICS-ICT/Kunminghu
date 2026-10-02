// SPDX-License-Identifier: MulanPSL-2.0
package top

import freechips.rocketchip.diplomacy.DisableMonitors
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import system.SoCParamsKey
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{DebugOptionsKey, XSTileKey}

/** Records final production parameters; ordinary SimTop provides no dedicated feature observations. */
object DasicsDevelopmentMain extends App {
  val retiredSelections = Seq("HasUserTimerInterrupt", "CONFIG_RV_USER_TIMER", "CONFIG_DIFFTEST_UIT",
    "UIT05_USER_TIMER", "UIT06_USER_TIMER", "UIT07_USER_TIMER")
  require(!retiredSelections.exists(sys.env.contains), "Independent UIT environment selection is retired")
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)
  val cores = config(XSTileKey)
  val options = config(DebugOptionsKey)
  val soc = config(SoCParamsKey)
  require(cores.size == 1, "The development mapping requires one hart")
  val core = cores.head
  require(!core.productElementNames.contains("HasUserTimerInterrupt"),
    "The production core must have only one feature parameter")
  require(core.XLEN == 64 && core.VLEN == 128 && core.HasVPU,
    "The development mapping requires RV64 and RVV with VLEN=128")
  require(core.HartId == 0 && core.RobSize > 0 && core.RenameWidth > 0)
  require(!options.FPGAPlatform && options.EnableDifftest,
    "The development mapping requires ordinary non-FPGA DiffTest")
  require(!options.EnableConstantin && !options.EnableChiselDB && !options.UseDRAMSim)
  val l2 = core.L2CacheParamsOpt.get
  val l3 = soc.L3CacheParamsOpt.get
  val l2Bytes = core.L2NBanks.toLong * l2.sets * l2.ways * l2.blockBytes
  val l3Bytes = soc.L3NBanks.toLong * l3.sets * l3.ways * l3.blockBytes
  require(l2Bytes == 256L * 1024 && l3Bytes == 768L * 1024,
    "The development mapping requires its recorded simulation cache sizes")

  Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
  ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
  // Instantiate the ordinary SimTop; its optional observation hook remains empty.
  Generator.execute(firrtlOpts, DisableMonitors(p => new SimTop()(p))(config), firtoolOpts)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")

  val actual = Json.obj(
    "schema_version" -> Json.fromInt(1),
    "compile_origin" -> Json.fromString("generated"),
    "production_has_fdi_parameter" -> Json.fromBoolean(core.productElementNames.contains("HasFDI")),
    "has_fdi" -> Json.fromBoolean(core.HasFDI),
    "dedicated_observation_hook" -> Json.fromBoolean(false),
    "emitter" -> Json.fromString("top.DasicsDevelopmentMain"),
    "top" -> Json.fromString("top.SimTop"),
    "cores" -> Json.fromInt(cores.size),
    "hart_id" -> Json.fromInt(core.HartId),
    "xlen" -> Json.fromInt(core.XLEN),
    "vlen" -> Json.fromInt(core.VLEN),
    "has_vpu" -> Json.fromBoolean(core.HasVPU),
    "fpga_platform" -> Json.fromBoolean(options.FPGAPlatform),
    "enable_difftest" -> Json.fromBoolean(options.EnableDifftest),
    "rob_entries" -> Json.fromInt(core.RobSize),
    "rename_width" -> Json.fromInt(core.RenameWidth),
    "commit_width" -> Json.fromInt(core.CommitWidth),
    "l2_bytes" -> Json.fromLong(l2Bytes),
    "l2_banks" -> Json.fromInt(core.L2NBanks),
    "l2_sets" -> Json.fromInt(l2.sets),
    "l2_ways" -> Json.fromInt(l2.ways),
    "l2_block_bytes" -> Json.fromInt(l2.blockBytes),
    "l3_bytes" -> Json.fromLong(l3Bytes),
    "l3_banks" -> Json.fromInt(soc.L3NBanks),
    "l3_sets" -> Json.fromInt(l3.sets),
    "l3_ways" -> Json.fromInt(l3.ways),
    "l3_block_bytes" -> Json.fromInt(l3.blockBytes),
    "core_parameters" -> Json.fromString(core.toString),
    "debug_parameters" -> Json.fromString(options.toString),
    "soc_parameters" -> Json.fromString(soc.toString)
  )
  Files.write(Paths.get("build", "dasics-generated-configuration.json"),
    (actual.spaces2 + "\n").getBytes(StandardCharsets.UTF_8),
    StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
}
