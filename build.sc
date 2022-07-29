import mill._
import scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt.ScalafmtModule
import $ivy.`io.github.davidgregory084::mill-tpolecat_mill0.10:0.3.0`
import io.github.davidgregory084.TpolecatModule
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.10:0.1.4`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`de.tototec::de.tobiasroeser.mill.integrationtest::0.6.0`
import de.tobiasroeser.mill.integrationtest.MillIntegrationTestModule

object parser extends Cross[ParserModule]("2.12.15", "2.13.8")
class ParserModule(val crossScalaVersion: String)
    extends BaseModule
    with CrossSbtModule
    with CommonPublishModule {
  override def ivyDeps = Agg(
    ivy"io.swagger.parser.v3:swagger-parser:2.0.28",
    ivy"com.softwaremill.sttp.model::core:1.5.0"
  )
  object test extends Tests with CommonTestModule
}

object core extends Cross[CoreModule]("2.12.15", "2.13.8")
class CoreModule(val crossScalaVersion: String)
    extends BaseModule
    with CrossSbtModule
    with CommonPublishModule {
  override def moduleDeps = Seq(parser())

  override def ivyDeps = Agg(
    ivy"org.scalameta::scalameta::4.5.8",
    ivy"org.typelevel::cats-core::2.8.0"
  )
  object test extends Tests with CommonTestModule
}

object `mill-codegen-plugin` extends Cross[MillCodegenPlugin]("2.13.8")
class MillCodegenPlugin(val crossScalaVersion: String)
    extends BaseModule
    with CrossScalaModule
    with CommonPublishModule {
  override def moduleDeps = Seq(core())

  val millVersion = "0.10.0" // scala-steward:off
  val millScalalib = ivy"com.lihaoyi::mill-scalalib:$millVersion"

  override def ivyDeps = Agg(
    millScalalib
  )
}

object `mill-codegen-plugin-itest` extends MillIntegrationTestModule {
  def millTestVersion = T {
    val ctx = T.ctx()
    ctx.env
      .get("TEST_MILL_VERSION")
      .filterNot(_.isEmpty)
      .getOrElse(BuildInfo.millVersion)
  }
  def pluginsUnderTest = Seq(`mill-codegen-plugin`("2.13.8"))
}

trait CommonTestModule extends BaseModule with TestModule {
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.8.0",
    ivy"com.softwaremill.diffx::diffx-utest::0.7.1",
    ivy"com.softwaremill.diffx::diffx-cats::0.7.1",
    ivy"com.softwaremill.sttp.client3::core::3.7.2",
    ivy"com.softwaremill.sttp.client3::circe::3.7.2",
    ivy"io.circe::circe-core::0.14.2",
    ivy"io.circe::circe-generic::0.14.2",
    ivy"io.circe::circe-parser::0.14.2",
    ivy"io.circe::circe-yaml::0.14.1"
  )
  override def testFramework = "utest.runner.Framework"
}

trait BaseModule extends ScalafmtModule with TpolecatModule {
  override def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xfatal-warnings"))
  }
}

trait CommonPublishModule extends PublishModule {
  def publishVersion = T {
    val vcsState = VcsVersion.vcsState()
    val formattedTag = vcsState.format(tagModifier =
      t =>
        if (t.startsWith("v")) {
          t.drop(1)
        } else {
          t
        }
    )
    if (vcsState.commitsSinceLastTag > 0) {
      s"$formattedTag-SNAPSHOT"
    } else {
      formattedTag
    }
  }
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "io.github.ghostbuster91.sttp-openapi",
    url = "https://github.com/ghostbuster91/sttp-openapi-generator",
    licenses = Seq(License.`Apache-2.0`),
    versionControl =
      VersionControl.github("ghostbuster91", "sttp-openapi-generator"),
    developers = Seq(
      Developer(
        "ghostbuster91",
        "Kasper Kondzielski",
        "https://github.com/ghostbuster91"
      )
    )
  )
}
