lazy val root = (project in file("."))
  .enablePlugins(SttpOpenApiCodegenPlugin)
  .settings(
    version := "0.1",
    scalaVersion := "2.13.5",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client3" %% "core" % "3.2.3",
      "com.softwaremill.sttp.client3" %% "circe" % "3.2.3",
      "io.circe" %% "circe-core" % "0.13.0",
      "io.circe" %% "circe-generic" % "0.13.0",
      "io.circe" %% "circe-parser" % "0.13.0",
    )
  )
