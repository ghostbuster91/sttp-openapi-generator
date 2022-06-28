import SttpOpenApiCodegenPlugin._

lazy val root = (project in file("."))
  .enablePlugins(SttpOpenApiCodegenPlugin)
  .settings(
    version := "0.1",
    scalaVersion := "3.1.2"
  )
