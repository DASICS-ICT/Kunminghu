import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("L03_SOURCE_ROOT"))
val runRoot = os.Path(sys.env("L03_RUN_ROOT"))

// Compile production source modules with the repository's pinned toolchain.
trait ProductionModule extends SbtModule {
  def scalaVersion = "2.13.15"
  override def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.chipsalliance::chisel:6.6.0")
  override def scalacPluginIvyDeps = Agg(ivy"org.chipsalliance:::chisel-plugin:6.6.0")
  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")
}

// Passive production hierarchy generation stays separate from local simulation.
object observed extends ProductionModule {
  override def sources = T.sources(Seq(
    "src/test/scala/top/SimTop.scala",
    "src/test/scala/top/SimMMIO.scala",
    "src/test/scala/xiangshan/mem/FDIStorePermissionObservedMain.scala")
    .map(path => PathRef(sourceRoot / os.RelPath(path))))
  override def moduleDeps = Seq(production)
  override def forkArgs = Seq("-Xmx48G", "-Xss256m")
  override def forkWorkingDir = runRoot
}

object rocketMacros extends ScalaModule {
  def scalaVersion = "2.13.15"
  override def sources = T.sources(Seq(PathRef(sourceRoot / "rocket-chip/macros/src/main/scala")))
  override def ivyDeps = Agg(ivy"org.scala-lang:scala-reflect:2.13.15")
}

object cde extends ScalaModule {
  def scalaVersion = "2.13.15"
  override def sources = T.sources(Seq(PathRef(sourceRoot / "rocket-chip/cde/cde/src")))
}

object hardfloat extends ProductionModule {
  override def millSourcePath = sourceRoot / "rocket-chip/hardfloat/hardfloat"
}

object rocketchip extends ProductionModule {
  override def millSourcePath = sourceRoot / "rocket-chip"
  override def moduleDeps = Seq(rocketMacros, cde, hardfloat)
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::mainargs:0.7.0",
    ivy"org.json4s::json4s-jackson:4.0.7")
}

object utility extends ProductionModule {
  override def millSourcePath = sourceRoot / "utility"
  override def moduleDeps = Seq(rocketchip)
  override def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::sourcecode:0.4.2")
}

object huancun extends ProductionModule {
  override def millSourcePath = sourceRoot / "huancun"
  override def moduleDeps = Seq(rocketchip, utility)
}

object coupledL2 extends ProductionModule {
  override def millSourcePath = sourceRoot / "coupledL2"
  override def moduleDeps = Seq(rocketchip, utility, huancun)
}

object openNCB extends ProductionModule {
  override def millSourcePath = sourceRoot / "openLLC/openNCB"
  override def moduleDeps = Seq(rocketchip)
}

object openLLC extends ProductionModule {
  override def millSourcePath = sourceRoot / "openLLC"
  override def moduleDeps = Seq(rocketchip, utility, coupledL2, openNCB)
}

object yunsuan extends ProductionModule {
  override def millSourcePath = sourceRoot / "yunsuan"
}

object fudian extends ProductionModule {
  override def millSourcePath = sourceRoot / "fudian"
}

object difftest extends ProductionModule {
  override def millSourcePath = sourceRoot / "difftest"
}

object chiselAIA extends ProductionModule {
  override def millSourcePath = sourceRoot / "ChiselAIA"
  override def moduleDeps = Seq(rocketchip, utility)
}

object macros extends ScalaModule {
  def scalaVersion = "2.13.15"
  override def sources = T.sources(Seq(PathRef(sourceRoot / "macros/src")))
  override def ivyDeps = Agg(ivy"org.scala-lang:scala-reflect:2.13.15")
}

object production extends ProductionModule {
  override def millSourcePath = sourceRoot
  override def moduleDeps = Seq(rocketchip, utility, huancun, coupledL2, openLLC,
    yunsuan, fudian, difftest, chiselAIA, macros)
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"edu.berkeley.cs::chiseltest:6.0.0",
    ivy"io.circe::circe-yaml:1.15.0",
    ivy"io.circe::circe-generic-extras:0.14.4")
  override def resources = T.sources {
    // Frozen candidate trees have no Git metadata and must not inherit the enclosing repository identity.
    require(os.isFile(sourceRoot / "candidate-revision") && os.isFile(sourceRoot / "candidate-dirty"),
      "Incomplete frozen source identity")
    val revision = os.read(sourceRoot / "candidate-revision").trim
    require(revision.nonEmpty, "Empty source revision")
    val dirty = os.read(sourceRoot / "candidate-dirty").trim
    require(Set("0", "1").contains(dirty), "Invalid source dirty state")
    val changed = dirty == "1"
    os.write(T.dest / "gitStatus", s"SHA=$revision\ndirty=${if (changed) 1 else 0}\n")
    os.write(T.dest / "publishVersion", s"Kunminghu-dev ${revision.take(8)}${if (changed) "-dirty" else ""}")
    super.resources() ++ Seq(PathRef(T.dest))
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    override def sources = T.sources {
      val selected = T.env.get("L03_SUITE").get match {
        case "compile" | "local-minimal" | "local-full" | "local-remaining" => Seq(
          "src/test/scala/xiangshan/mem/FDIStorePermissionHarness.scala",
          "src/test/scala/xiangshan/mem/FDIStorePermissionTest.scala")
        case "backend-minimal" | "backend-full" | "backend-source-mode" => Seq(
          "src/test/scala/xiangshan/backend/fu/FDIStorePermissionBackendHarness.scala",
          "src/test/scala/xiangshan/backend/fu/FDIStorePermissionBackendTest.scala",
          "src/test/scala/xiangshan/mem/FDIStorePermissionHarness.scala",
          "src/test/scala/xiangshan/backend/fu/UserTimerDeliveryHarness.scala")
        case "misalign-revoke" => Seq(
          "src/test/scala/xiangshan/mem/FDIStoreMisalignRevokeTest.scala")
        case other => throw new IllegalArgumentException(s"No test sources for suite: $other")
      }
      selected.map { relative =>
        val source = sourceRoot / os.RelPath(relative)
        require(os.isFile(source), s"Missing released L03 source: $relative")
        PathRef(source)
      }
    }
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = T {
      Seq(s"-Xmx${T.env.get("L03_TEST_HEAP").get}", s"-Xss${T.env.get("L03_TEST_STACK").get}",
        s"-Dl03.runRoot=$runRoot")
    }
    // Verilator 5.020 PCH flags conflict with the additional harness include.
    override def forkEnv = T {
      super.forkEnv() ++ Map(
        "MAKEFLAGS" -> s"-j${T.env.get("DASICS_JOBS").get} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "L03_FDI_ENABLED" -> T.env.get("L03_FDI_ENABLED").get,
        "L03_SCENARIO" -> T.env.get("L03_SCENARIO").get)
    }
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = runRoot
  }
}
