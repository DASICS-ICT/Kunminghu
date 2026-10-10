// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend

class FDIFullControlBackendTest extends AnyFlatSpec {
  behavior of "DASICS production control-flow completion"

  it should "preserve precise call and target effects through the real Backend" in {
    val root = Paths.get(sys.env("FDI_CONTROL_RUN_ROOT")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated evidence directory")
    val enabled = sys.env.getOrElse("FDI_CONTROL_ENABLED", "true").toBoolean
    val tasks = sys.env.getOrElse("FDI_CONTROL_TASKS", "f02").split(",").toSeq
    require(tasks.nonEmpty && tasks.distinct.size == tasks.size && tasks.forall(Set("f02")))
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
    val options = p(xiangshan.DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(root.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDIFullControlHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      tasks.foreach { task =>
        val path = root.resolve(task)
        require(!Files.exists(path), s"Never replace an earlier task result: $path")
        Files.createDirectory(path)
        val driver = new FDIFullControlDriver(elaborated.wrapped, path)
        var error: Option[Throwable] = None
        try {
          task match {
            case "f02" => F02ControlPrograms.run(driver)
          }
        } catch { case failure: Throwable => error = Some(failure); throw failure }
        finally driver.finish(error)
      }
    }
  }
}
