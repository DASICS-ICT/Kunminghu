import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("UIT02_SOURCE_ROOT"))

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
    val revision = os.proc("git", "rev-parse", "HEAD").call(cwd = sourceRoot).out.text().trim
    val changed = os.proc("git", "status", "--porcelain").call(cwd = sourceRoot).out.text().nonEmpty
    os.write(T.dest / "gitStatus", s"SHA=$revision\ndirty=${if (changed) 1 else 0}\n")
    // Full-core DTS metadata requires a version resource; this scoped build is never a release.
    os.write(T.dest / "publishVersion", s"Kunminghu-dev ${revision.take(8)}${if (changed) "-dirty" else ""}")
    super.resources() ++ Seq(PathRef(T.dest))
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    // Each invocation may select a different evidence directory while retaining compile caches.
    def runDirectory = T.input {
      val environment = T.env
      os.Path(environment.getOrElse("UIT04_RUN_ROOT", environment("UIT02_RUN_ROOT")))
    }
    override def sources = T.sources {
      Seq("UserTimerCSRTest.scala", "UserTimerCSRIntegrationTest.scala", "UserTimerDecodeTest.scala", "UserTimerTest.scala",
        "UserTimerEntryReturnTest.scala", "UserTimerDeliveryHarness.scala", "UserTimerDeliveryTest.scala")
        .map(name => PathRef(sourceRoot / "src/test/scala/xiangshan/backend/fu" / name))
    }
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = T {
      val runRoot = runDirectory()
      Seq("-Xmx40G", "-Xss256m", s"-Duit02.runRoot=$runRoot", s"-Duit01.runRoot=$runRoot",
        s"-Duit03.runRoot=$runRoot", s"-Duit04.runRoot=$runRoot")
    }
    // Verilator 5.020's generated PCH flags conflict with the additional harness include.
    override def forkEnv = T {
      super.forkEnv() ++ Map("MAKEFLAGS" -> "VK_PCH_I_FAST= VK_PCH_I_SLOW=")
    }
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = T { runDirectory() }
  }
}
