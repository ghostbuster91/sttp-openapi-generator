import mill._, scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt.ScalafmtModule
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion

object core extends BaseModule with SbtModule {
  def scalaVersion = "2.13.6"

  def ivyDeps = Agg(
    ivy"org.scalameta::scalameta::4.4.22",
    ivy"io.swagger.parser.v3:swagger-parser:2.0.25",
    ivy"com.softwaremill.sttp.model::core:1.4.7",
    ivy"org.typelevel::cats-core::2.6.1"
  )
  object test extends Tests with CommonTestModule
}

trait CommonTestModule extends BaseModule with TestModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.7.10",
    ivy"com.softwaremill.diffx::diffx-utest::0.5.2",
    ivy"com.softwaremill.diffx::diffx-cats::0.5.2",
    ivy"com.softwaremill.sttp.client3::core::3.3.7",
    ivy"com.softwaremill.sttp.client3::circe::3.3.7",
    ivy"io.circe::circe-core::0.13.0",
    ivy"io.circe::circe-generic::0.13.0",
    ivy"io.circe::circe-parser::0.13.0",
    ivy"io.circe::circe-yaml::0.14.0"
  )
  def testFrameworks = Seq("utest.runner.Framework")
}

trait BaseModule extends ScalafmtModule with TpolecatModule {
  def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xfatal-warnings"))
  }
}
