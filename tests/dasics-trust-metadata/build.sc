import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("C08_SOURCE_ROOT"))
val runRoot = os.Path(sys.env("C08_RUN_ROOT"))

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
    override def sources = T.sources {
      val frontend = Seq(
        "src/test/scala/xiangshan/frontend/FDITrustMetadataFrontendHarness.scala",
        "src/test/scala/xiangshan/frontend/FDITrustMetadataFrontendTest.scala")
      val backend = Seq(
        "src/test/scala/xiangshan/backend/fu/FDITrustMetadataBackendHarness.scala",
        "src/test/scala/xiangshan/backend/fu/FDITrustMetadataBackendTest.scala")
      val recovery = Seq(
        "src/test/scala/xiangshan/backend/fu/FDITrustMetadataRecoveryHarness.scala",
        "src/test/scala/xiangshan/backend/fu/FDITrustMetadataRecoveryTest.scala")
      val suite = T.env.get("C08_SUITE").get
      def complete(paths: Seq[String]): Boolean = {
        val present = paths.count(path => os.isFile(sourceRoot / os.RelPath(path)))
        require(present == 0 || present == paths.size, "Incomplete C08 test source pair")
        present == paths.size
      }
      val selectRecovery = suite.startsWith("recovery-") || (suite == "compile" && complete(recovery))
      // Recovery reuses the instruction-image helper without requiring the frontend test suite.
      val frontendHelperOnly = selectRecovery && os.isFile(sourceRoot / os.RelPath(frontend.head)) &&
        !os.isFile(sourceRoot / os.RelPath(frontend.last))
      val selectFrontend = suite.startsWith("front-") ||
        (suite == "compile" && !frontendHelperOnly && complete(frontend))
      val selectBackend = suite.startsWith("back-") || (suite == "compile" && complete(backend))
      require(selectFrontend || selectBackend || selectRecovery, "No C08 test source pair is available")
      // Backend fixtures use only the released committed support closure.
      val support = if (selectBackend || selectRecovery)
        os.read.lines(sourceRoot / "c08-backend-support.txt").filter(_.nonEmpty)
        else Seq.empty[String]
      val selected = (if (selectFrontend) frontend else Seq.empty[String]) ++
        (if (selectBackend) backend else Seq.empty[String]) ++
        (if (selectRecovery) recovery ++ Seq(frontend.head) else Seq.empty[String]) ++ support
      selected.distinct.map { path =>
        val file = sourceRoot / os.RelPath(path)
        require(os.isFile(file), s"Missing selected C08 test source: $path")
        PathRef(file)
      }
    }
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = T {
      Seq(s"-Xmx${T.env.get("C08_TEST_HEAP").get}", s"-Xss${T.env.get("C08_TEST_STACK").get}",
        s"-Dc08.runRoot=$runRoot")
    }
    // Verilator 5.020's generated PCH flags conflict with the additional harness include.
    override def forkEnv = T {
      super.forkEnv() ++ Map(
        "MAKEFLAGS" -> s"-j${T.env.get("DASICS_JOBS").get} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "C08_FDI_ENABLED" -> T.env.get("C08_FDI_ENABLED").get,
        "C08_FRONTEND_SCENARIO" -> T.env.get("C08_FRONTEND_SCENARIO").get,
        "C08_BACKEND_SCENARIO" -> T.env.get("C08_BACKEND_SCENARIO").get,
        "C08_RECOVERY_SCENARIO" -> T.env.get("C08_RECOVERY_SCENARIO").get)
    }
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = runRoot
  }
}

// Keep structural observations in a separate module from the simulation fixtures.
object observed extends ProductionModule {
  override def sources = T.sources(Seq(
    "src/test/scala/top/SimTop.scala",
    "src/test/scala/top/SimMMIO.scala",
    "src/test/scala/xiangshan/frontend/FDITrustMetadataObservedMain.scala")
    .map(path => PathRef(sourceRoot / os.RelPath(path))))
  override def moduleDeps = Seq(production)
  override def forkArgs = Seq("-Xmx48G", "-Xss256m")
  override def forkWorkingDir = runRoot
}
