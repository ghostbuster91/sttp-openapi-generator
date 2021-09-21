import SttpOpenApiCodegenPlugin._

lazy val root = (project in file("."))
  .enablePlugins(SttpOpenApiCodegenPlugin)
  .settings(
    version := "0.1",
    scalaVersion := "2.12.15",
    sttpOpenApiInput := Seq(
      Input.SingleFile(
        (Compile / resourceDirectory).value / "openapi.yaml",
        pkg = "com.example.openapi"
      ),
      Input.SingleFile(
        (Compile / resourceDirectory).value / "otherYamlFile.yaml",
        pkg = "com.example.other"
      )
    )
  )
