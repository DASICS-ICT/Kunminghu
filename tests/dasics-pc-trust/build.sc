import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("P03_SOURCE_ROOT"))
val runRoot = os.Path(sys.env("P03_RUN_ROOT"))
val jobs = sys.env.getOrElse("DASICS_JOBS", "48").toInt
require(jobs > 0, "DASICS_JOBS must be positive")

// Compile only the PC classifier and its pure comparison dependencies.
object pcTrust extends ScalaModule {
  def scalaVersion = "2.13.15"
  override def ivyDeps = Agg(ivy"org.chipsalliance::chisel:6.6.0")
  override def scalacPluginIvyDeps = Agg(ivy"org.chipsalliance:::chisel-plugin:6.6.0")
  override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
  override def sources = T.sources {
    Seq(
      PathRef(sourceRoot / "src/main/scala/xiangshan/backend/fu/FDISourcePrivilege.scala"),
      PathRef(sourceRoot / "src/main/scala/xiangshan/backend/fu/FDIBoundChecker.scala"),
      PathRef(sourceRoot / "src/main/scala/xiangshan/backend/fu/FDIPcTrustChecker.scala")
    )
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def sources = T.sources {
      Seq(PathRef(sourceRoot / "src/test/scala/xiangshan/backend/fu/FDIPcTrustCheckerTest.scala"))
    }
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = Seq("-Xmx4G", s"-XX:ActiveProcessorCount=$jobs", s"-Dp03.runRoot=$runRoot")
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = runRoot
  }
}
