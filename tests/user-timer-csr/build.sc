import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("UIT02_SOURCE_ROOT"))
val runRoot = os.Path(sys.env("UIT02_RUN_ROOT"))

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
    val frozenIdentity = os.exists(sourceRoot / "candidate-revision")
    require(frozenIdentity == os.exists(sourceRoot / "candidate-dirty"), "Incomplete frozen source identity")
    require(frozenIdentity || os.exists(sourceRoot / ".git"), "Source root has no local Git or frozen identity")
    val revision = if (frozenIdentity) os.read(sourceRoot / "candidate-revision").trim
      else os.proc("git", "rev-parse", "HEAD").call(cwd = sourceRoot).out.text().trim
    require(revision.nonEmpty, "Empty source revision")
    val dirty = if (frozenIdentity) os.read(sourceRoot / "candidate-dirty").trim
      else if (os.proc("git", "status", "--porcelain").call(cwd = sourceRoot).out.text().nonEmpty) "1" else "0"
    require(Set("0", "1").contains(dirty), "Invalid source dirty state")
    val changed = dirty == "1"
    os.write(T.dest / "gitStatus", s"SHA=$revision\ndirty=${if (changed) 1 else 0}\n")
    super.resources() ++ Seq(PathRef(T.dest))
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    override def sources = T.sources {
      Seq("HasFDIConfigurationTest.scala", "UserTimerCSRTest.scala", "UserTimerCSRIntegrationTest.scala", "UserTimerDecodeTest.scala", "UserTimerTest.scala")
        .map(name => PathRef(sourceRoot / "src/test/scala/xiangshan/backend/fu" / name))
    }
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = Seq("-Xmx12G", "-Xss32m", s"-Duit02.runRoot=$runRoot", s"-Duit01.runRoot=$runRoot")
    // Verilator 5.020's generated PCH flags conflict with the additional harness include.
    override def forkEnv = T {
      super.forkEnv() ++ Map("MAKEFLAGS" -> "VK_PCH_I_FAST= VK_PCH_I_SLOW=")
    }
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = runRoot
  }
}
