import mill._
import mill.scalalib._

val sourceRoot = os.Path(sys.env("P02_SOURCE_ROOT"))
val runRoot = os.Path(sys.env("P02_RUN_ROOT"))

// Compile the aggregate and its single-entry dependency without the production core.
object boundsChecker extends ScalaModule {
  def scalaVersion = "2.13.15"
  override def ivyDeps = Agg(ivy"org.chipsalliance::chisel:6.6.0")
  override def scalacPluginIvyDeps = Agg(ivy"org.chipsalliance:::chisel-plugin:6.6.0")
  override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
  override def sources = T.sources {
    Seq(
      PathRef(sourceRoot / "src/main/scala/xiangshan/backend/fu/FDIBoundChecker.scala"),
      PathRef(sourceRoot / "src/main/scala/xiangshan/backend/fu/FDIBoundsChecker.scala")
    )
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(ivy"edu.berkeley.cs::chiseltest:6.0.0")
    override def sources = T.sources {
      Seq(PathRef(sourceRoot / "src/test/scala/xiangshan/backend/fu/FDIBoundsCheckerTest.scala"))
    }
    override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls")
    override def forkArgs = Seq("-Xmx4G", s"-Dp02.runRoot=$runRoot")
    override def testSandboxWorkingDir = false
    override def forkWorkingDir = runRoot
  }
}
