import mill._, scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt.ScalafmtModule
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule
import $ivy.`com.goyeau::mill-scalafix:0.2.1`
import com.goyeau.mill.scalafix.ScalafixModule
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion

object core extends BaseModule {
  def ivyDeps = Agg(ivy"org.typelevel::cats-parse::0.2.0")
  object test extends Tests with CommonTestModule
}


trait CommonTestModule extends BaseModule with TestModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.7.7",
    ivy"com.softwaremill.diffx::diffx-utest::0.4.4",
    ivy"com.softwaremill.diffx::diffx-cats::0.4.4"
  )
  def testFrameworks = Seq("utest.runner.Framework")
}

trait BaseModule
    extends ScalaModule
    with ScalafmtModule
    with TpolecatModule
    with ScalafixModule {
  def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xfatal-warnings")) ++ Seq(
      "-Ymacro-annotations"
    )
  }
}

