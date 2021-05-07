import SttpOpenApiCodegenPlugin._
val scalatestVersion = "3.2.8"

lazy val root = (project in file("."))
  .enablePlugins(SttpOpenApiCodegenPlugin)
  .settings(
    version := "0.1",
    scalaVersion := "2.12.13",
    Test / fork := true,
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client3" %% "okhttp-backend" % "3.3.0", //TODO should it match with plugin's sttp version?
      "com.softwaremill.diffx" %% "diffx-scalatest" % "0.4.5" % Test,
      "org.scalatest" %% "scalatest-flatspec" % scalatestVersion % Test,
      "org.scalatest" %% "scalatest-shouldmatchers" % scalatestVersion % Test,
      "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.39.3" % Test
    )
  )
