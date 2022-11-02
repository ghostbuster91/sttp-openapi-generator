val Scala212 = "2.12.15"

ThisBuild / scalaVersion := Scala212

val commonSettings = Seq(
  organization := "io.github.ghostbuster91.sttp-openapi",
  homepage := Some(
    url("https://github.com/ghostbuster91/sttp-openapi-generator")
  ),
  licenses := List(
    "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
  ),
  developers := List(
    Developer(
      "ghostbuster91",
      "Kasper Kondzielski",
      "kghost0@gmail.com",
      url("https://github.com/ghostbuster91")
    )
  ),
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeProfileName := "io.github.ghostbuster91",
  scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings"))),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  addCompilerPlugin(
    ("org.scalameta" % "semanticdb-scalac" % "4.5.8").cross(CrossVersion.full)
  )
)

lazy val rootProject = (project in file("."))
  .settings(commonSettings)
  .settings(
    name := "sttp-openapi-generator",
    publish / skip := true
  )
  .aggregate(core, codegenSbtPlugin, parser)

lazy val testDependencies = Seq(
  "com.lihaoyi" %% "utest" % "0.8.1",
  "com.softwaremill.diffx" %% "diffx-utest" % "0.7.1",
  "com.softwaremill.diffx" %% "diffx-cats" % "0.7.1",
  "com.softwaremill.sttp.client3" %% "core" % "3.7.6",
  "com.softwaremill.sttp.client3" %% "circe" % "3.7.6",
  "io.circe" %% "circe-core" % "0.14.2",
  "io.circe" %% "circe-generic" % "0.14.2",
  "io.circe" %% "circe-parser" % "0.14.2",
  "io.circe" %% "circe-yaml" % "0.14.2"
).map(_ % Test)

lazy val parser: Project = (project in file("parser"))
  .settings(commonSettings)
  .settings(
    name := "parser",
    libraryDependencies ++= Seq(
      "io.swagger.parser.v3" % "swagger-parser" % "2.0.28",
      "com.softwaremill.sttp.model" %% "core" % "1.5.2"
    ) ++ testDependencies
  )

lazy val core: Project = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "codegen-core",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "4.5.8",
      "org.typelevel" %% "cats-core" % "2.8.0"
    ) ++ testDependencies
  )
  .dependsOn(parser)

lazy val codegenSbtPlugin: Project = (project in file("sbt-codegen-plugin"))
  .enablePlugins(SbtPlugin)
  .settings(commonSettings)
  .settings(
    name := "sbt-codegen-plugin",
    sbtPlugin := true,
    scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
    },
    libraryDependencies += "org.scalameta" %% "scalafmt-dynamic" % "3.0.3",
    scripted := {
      val x = (core / publishLocal).value
      scripted.evaluated
    }
  )
  .dependsOn(core)
