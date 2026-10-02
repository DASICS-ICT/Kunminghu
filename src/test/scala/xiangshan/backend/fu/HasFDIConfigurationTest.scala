// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.scalatest.flatspec.AnyFlatSpec
import top.ArgParser
import xiangshan.{XSCoreParameters, XSTileKey}

class HasFDIConfigurationTest extends AnyFlatSpec {
  behavior of "Production feature configuration"

  private def cores(arguments: String*): Seq[XSCoreParameters] = {
    val (config, firrtlOptions, _) = ArgParser.parse(arguments.toArray)
    assert(firrtlOptions.isEmpty, "Feature selections must be consumed by the production parser")
    config(XSTileKey)
  }

  private def withYaml(contents: String)(body: String => Unit): Unit = {
    val path = Files.createTempFile(Paths.get(sys.props("uit02.runRoot")), "feature-config-", ".yaml")
    try {
      Files.write(path, contents.getBytes(StandardCharsets.UTF_8))
      body(path.toString)
    } finally Files.delete(path)
  }

  it should "inherit the disabled default in ordinary production configurations" in {
    assert(!XSCoreParameters().HasFDI)
    for (name <- Seq("DefaultConfig", "MinimalConfig", "FpgaDefaultConfig", "FpgaDiffDefaultConfig")) {
      val default = cores("--config", name)
      val explicitOff = cores("--config", name, "--has-fdi", "false")
      assert(default.nonEmpty && default.forall(!_.HasFDI))
      assert(explicitOff.size == default.size && explicitOff.forall(!_.HasFDI))
    }
  }

  it should "apply either feature value after class selection and core replication" in {
    for (enabled <- Seq(false, true);
         arguments <- Seq(
           Seq("--has-fdi", enabled.toString, "--config", "FpgaDefaultConfig", "--num-cores", "2"),
           Seq("--config", "FpgaDefaultConfig", "--has-fdi", enabled.toString, "--num-cores", "2"),
           Seq("--config", "FpgaDefaultConfig", "--num-cores", "2", "--has-fdi", enabled.toString))) {
      val selected = cores(arguments: _*)
      assert(selected.size == 2 && selected.map(_.HartId) == Seq(0, 1))
      assert(selected.forall(core => core.HasFDI == enabled && core.HasVPU && core.VLEN == 128))
      assert(selected.forall(core => core.RobSize > 1 && core.RenameWidth > 1))
    }
  }

  it should "retain the feature selection across a later YAML configuration replacement" in {
    withYaml("Config: FpgaDefaultConfig\n") { path =>
      for (enabled <- Seq(false, true)) {
        val selected = cores("--has-fdi", enabled.toString, "--yaml-config", path, "--num-cores", "2")
        assert(selected.size == 2 && selected.map(_.HartId) == Seq(0, 1))
        assert(selected.forall(core => core.HasFDI == enabled && core.HasVPU && core.VLEN == 128))
      }
    }
  }

  it should "reject missing malformed and repeated feature values" in {
    val invalid = Seq(
      Seq("--has-fdi"), Seq("--has-fdi", "TRUE"), Seq("--has-fdi", "1"),
      Seq("--has-fdi", "yes"), Seq("--has-fdi=true"),
      Seq("HasFDI=true"), Seq("--HasFDI", "false"),
      Seq("--has-fdi", "true", "--has-fdi", "true"),
      Seq("--has-fdi", "false", "--config", "FpgaDefaultConfig", "--has-fdi", "true"))
    invalid.foreach { arguments =>
      withClue(arguments.mkString(" ")) {
        intercept[IllegalArgumentException] { ArgParser.parse(arguments.toArray) }
      }
    }
  }

  it should "reject independent legacy command line feature selections" in {
    for (option <- Seq("--has-user-timer-interrupt", "--user-timer-interrupt", "--HasUserTimerInterrupt",
         "HasUserTimerInterrupt");
         arguments <- Seq(Array(option, "false"), Array(s"$option=false"))) {
      intercept[IllegalArgumentException] { ArgParser.parse(arguments) }
    }
  }

  it should "reject unsupported feature fields instead of silently ignoring them in YAML" in {
    for (field <- Seq("HasFDI", "HasUserTimerInterrupt", "UIT", "CONFIG_RV_USER_TIMER", "CONFIG_DIFFTEST_UIT")) {
      withYaml(s"Config: FpgaDefaultConfig\n$field: false\n") { path =>
        intercept[IllegalArgumentException] { ArgParser.parse(Array("--yaml-config", path)) }
      }
    }
  }
}
