import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("DASICS_SOURCE_ROOT"))

// Compile production source modules with the repository's pinned toolchain.
trait ProductionModule extends SbtModule {
  def scalaVersion = "2.13.15"
  override def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.chipsalliance::chisel:6.6.0")
  override def scalacPluginIvyDeps = Agg(ivy"org.chipsalliance:::chisel-plugin:6.6.0")
  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")
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
    // Attempt paths and feature selectors must invalidate cached fork settings.
    def runtimeEnvironment = T.input {
      val keys = Seq("DASICS_RUN_ROOT", "DASICS_TEST_HEAP", "DASICS_TEST_STACK", "DASICS_TEST_SOURCES",
        "DASICS_JOBS", "L01_FDI_ENABLED", "FDI_CONTROL_ENABLED", "FDI_CONTROL_RUN_ROOT", "FDI_CONTROL_TASKS",
        "L04_FDI_ENABLED", "L04_SCENARIO", "NOOP_HOME", "BUILD_DIR", "TMPDIR", "JAVA_TOOL_OPTIONS",
        "GIT_CEILING_DIRECTORIES")
      val environment: Map[String, String] = keys.flatMap(key => T.env.get(key).map(key -> _)).toMap
      environment
    }
    override def sources = T.sources {
      val environment = runtimeEnvironment()
      val sources = ujson.read(environment("DASICS_TEST_SOURCES"))
      sources.arr.toSeq.map { entry =>
        val source = sourceRoot / os.RelPath(entry.str)
        require(os.isFile(source), s"Missing frozen test source: $source")
        PathRef(source)
      }
    }
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = T {
      val environment = runtimeEnvironment()
      val runRoot = environment("DASICS_RUN_ROOT")
      Seq(s"-Xmx${environment("DASICS_TEST_HEAP")}", s"-Xss${environment("DASICS_TEST_STACK")}",
        s"-Dl01.runRoot=$runRoot", s"-Df02.runRoot=$runRoot")
    }
    // Verilator 5.020 PCH flags conflict with the additional harness include.
    override def forkEnv = T {
      val environment = runtimeEnvironment()
      super.forkEnv() ++ environment ++ Map(
        "MAKEFLAGS" -> s"-j${environment("DASICS_JOBS")} VK_PCH_I_FAST= VK_PCH_I_SLOW=")
    }
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = T {
      val environment = runtimeEnvironment()
      os.Path(environment("DASICS_RUN_ROOT"))
    }
  }
}
