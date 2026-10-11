// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import io.circe.Json
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.jdk.CollectionConverters._
import xiangshan._

class FDILoadMisalignTimingEmitTest extends AnyFlatSpec {
  behavior of "DASICS misaligned-load synthesis view"

  it should "emit the production load fixture with verification-only removal" in {
    val root = Paths.get(sys.props("l04.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated evidence directory")
    val feature = sys.env("L04_FDI_ENABLED")
    require(Set("true", "false").contains(feature), "The feature setting must be explicit")
    val enabled = feature.toBoolean
    // Match the frozen functional fixture's configuration, including disabled L2L forwarding.
    val parameterArgs = Array("--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
      "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb")
    val (base, _, _) = top.ArgParser.parse(parameterArgs)
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
    }
    require(p(XSCoreParamsKey).HasVPU && p(XSCoreParamsKey).VLEN == 128)
    require(!p(XSCoreParamsKey).EnableLoadToLoadForward)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)

    val output = root.resolve("synthesis-rtl")
    val recordPath = root.resolve("synthesis-view.json")
    require(!Files.exists(output) && !Files.exists(recordPath), "Never replace an earlier synthesis view")
    Files.createDirectory(output)
    // Standard production emission removes verification nodes before generating SV.
    // The original functional test and its generated simulation RTL are separate inputs.
    val chiselArgs = Array("--target", "systemverilog", "--split-verilog",
      "--target-dir", output.toString, "--remove-assert")
    val firtoolOptions = Array("-disable-annotation-unknown")
    top.Generator.execute(chiselArgs, new FDILoadMisalignHarness, firtoolOptions)

    def sourceList(path: Path): Seq[Path] =
      Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.map(_.trim).filter(_.nonEmpty).map { line =>
        val source = output.resolve(line).normalize()
        require(source.startsWith(output) && Files.isRegularFile(source), s"Missing local RTL resource: $line")
        require(source.toString.endsWith(".sv") || source.toString.endsWith(".v"), s"Unexpected RTL resource: $line")
        source
      }

    val topFile = output.resolve("FDILoadMisalignHarness.sv")
    val filelist = output.resolve("filelist.f")
    val resources = output.resolve("firrtl_black_box_resource_files.f")
    require(Files.isRegularFile(topFile) && Files.isRegularFile(filelist), "Incomplete production RTL emission")
    val rtl = (sourceList(filelist) ++ (if (Files.isRegularFile(resources)) sourceList(resources) else Seq.empty)).distinct
    require(rtl.contains(topFile), "The source list must include the actual fixture top")

    def strings(values: Seq[String]): Json = Json.arr(values.map(Json.fromString): _*)
    val record = Json.obj(
      "schema_version" -> Json.fromInt(1),
      "status" -> Json.fromString("SYNTHESIS_VIEW_EMITTED"),
      "top" -> Json.fromString("FDILoadMisalignHarness"),
      "feature" -> Json.fromString(if (enabled) "on" else "off"),
      "parameter_factory" -> Json.fromString("FDILoadMisalignTest configuration"),
      "parameter_args" -> strings(parameterArgs.toSeq),
      "vector_length_bits" -> Json.fromInt(128),
      "load_to_load_forwarding" -> Json.fromBoolean(false),
      "generator" -> Json.fromString("top.Generator / top.XiangShanStage"),
      "verification_transform" -> Json.fromString("xiangshan.transforms.PrintControl / RemoveAssertAnnotation"),
      "removed_statement_kinds" -> strings(Seq("firrtl.ir.Stop", "firrtl.ir.Verification")),
      "chisel_args" -> strings(chiselArgs.toSeq),
      "firtool_options" -> strings(firtoolOptions.toSeq),
      "chisel_version" -> Json.fromString("6.6.0"),
      "firtool_directory" -> Json.fromString(sys.env("CHISEL_FIRTOOL_PATH")),
      "simulation_rtl_modified" -> Json.fromBoolean(false),
      "simulation_assertions_disabled" -> Json.fromBoolean(false),
      "relation_to_simulation" -> Json.fromString("Same frozen fixture and functional parameter configuration; standard verification-only synthesis view, not byte-identical ChiselTest simulation RTL"),
      "rtl_files" -> strings(rtl.map(path => output.relativize(path).toString)),
      "scope" -> Json.fromString("Production elaboration and synthesis-view emission only; no simulation or Vivado synthesis"))
    Files.write(recordPath, (record.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
  }
}
