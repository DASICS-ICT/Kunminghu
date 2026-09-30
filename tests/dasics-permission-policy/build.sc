import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("P04_SOURCE_ROOT"))
val runRoot = os.Path(sys.env("P04_RUN_ROOT"))
val jobs = sys.env.getOrElse("DASICS_JOBS", "1").toInt
require(jobs > 0, "DASICS_JOBS must be positive")

// Keep the policy and its integration harness independent of the production core.
object permissionPolicy extends ScalaModule {
  def scalaVersion = "2.13.15"
  override def ivyDeps = Agg(ivy"org.chipsalliance::chisel:6.6.0")
  override def scalacPluginIvyDeps = Agg(ivy"org.chipsalliance:::chisel-plugin:6.6.0")
  override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
  override def sources = T.sources {
    Seq("FDIBoundChecker.scala", "FDIBoundsChecker.scala", "FDISourcePrivilege.scala",
      "FDIPermissionPolicy.scala").map { name =>
      PathRef(sourceRoot / "src/main/scala/xiangshan/backend/fu" / name)
    }
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def sources = T.sources {
      Seq(PathRef(sourceRoot / "src/test/scala/xiangshan/backend/fu/FDIPermissionPolicyTest.scala"))
    }
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = Seq("-Xmx4G", s"-XX:ActiveProcessorCount=$jobs", s"-Dp04.runRoot=$runRoot")
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = runRoot
  }
}
