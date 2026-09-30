import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("P01_SOURCE_ROOT"))
val runRoot = os.Path(sys.env("P01_RUN_ROOT"))

// Compile only the standalone checker with the repository's existing toolchain.
object boundChecker extends ScalaModule {
  def scalaVersion = "2.13.15"
  override def ivyDeps = Agg(ivy"org.chipsalliance::chisel:6.6.0")
  override def scalacPluginIvyDeps = Agg(ivy"org.chipsalliance:::chisel-plugin:6.6.0")
  override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
  override def sources = T.sources {
    Seq(PathRef(sourceRoot / "src/main/scala/xiangshan/backend/fu/FDIBoundChecker.scala"))
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def sources = T.sources {
      Seq(PathRef(sourceRoot / "src/test/scala/xiangshan/backend/fu/FDIBoundCheckerTest.scala"))
    }
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = Seq("-Xmx4G", s"-Dp01.runRoot=$runRoot")
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = runRoot
  }
}
