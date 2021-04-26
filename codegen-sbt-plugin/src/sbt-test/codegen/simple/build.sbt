import SttpOpenApiCodegenPlugin._

lazy val root = (project in file("."))
  .enablePlugins(SttpOpenApiCodegenPlugin)
  .settings(
    version := "0.1",
    scalaVersion := "2.12.13",
    sttpOpenApiPackageName := "io.github.ghostbuster91.sttp.client3.example"
  )
