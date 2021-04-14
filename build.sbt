val Scala212 = "2.13.5"

val commonSettings = Seq(
  organization := "io.github.ghostbuster91.sttp3-openapi3",
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
  scalacOptions ~= (_.filterNot(Set("-Xfatal-warnings"))),
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val rootProject = (project in file("."))
  .settings(commonSettings)
  .settings(
    name := "sttp-openapi-generator"
  )
  .aggregate(core, codegenSbtPlugin)

lazy val testDependencies = Seq(
  "com.lihaoyi" %% "utest" % "0.7.7",
  "com.softwaremill.diffx" %% "diffx-utest" % "0.4.4",
  "com.softwaremill.diffx" %% "diffx-cats" % "0.4.4",
  "com.softwaremill.sttp.client3" %% "core" % "3.1.9",
  "com.softwaremill.sttp.client3" %% "circe" % "3.1.9",
  "io.circe" %% "circe-core" % "0.13.0",
  "io.circe" %% "circe-generic" % "0.13.0",
  "io.circe" %% "circe-parser" % "0.13.0",
  "io.circe" %% "circe-yaml" % "0.12.0"
).map(_ % Test)

lazy val core: Project = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "codegen-core",
    scalaVersion := Scala212,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "4.3.21",
      "io.swagger.parser.v3" % "swagger-parser" % "2.0.24"
    ) ++ testDependencies
  )

lazy val codegenSbtPlugin: Project = (project in file("codegen-sbt-plugin"))
  .enablePlugins(SbtPlugin)
  .settings(commonSettings)
  .settings(
    name := "sbt-codegen-plugin-circe",
    sbtPlugin := true,
    scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
    },
    libraryDependencies += "org.scalameta" %% "scalafmt-dynamic" % "2.7.5",
    scripted := {
      val x = (core / publishLocal).value
      scripted.evaluated
    }
  )
  .dependsOn(core)
