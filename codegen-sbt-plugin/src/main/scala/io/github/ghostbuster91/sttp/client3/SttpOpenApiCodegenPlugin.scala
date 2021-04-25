package io.github.ghostbuster91.sttp.client3

import sbt.{AutoPlugin, Def, File}
import sbt.Keys._
import sbt._
import org.scalafmt.interfaces.Scalafmt

import java.nio.file.Paths

object SttpOpenApiCodegenPlugin extends AutoPlugin {

  object autoImport {
    lazy val generateSources =
      Def.task {
        val log = streams.value.log
        val codegen = new Codegen(
          new SbtLogAdapter(log),
          CodegenConfig(handleErrors = false)
        )

        val sourcesDir = (Compile / sourceManaged).value
        val scalaVer = scalaVersion.value
        val swaggerDir = (Compile / resourceDirectory).value / "openapi.yaml"
        val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
        val cachedFun = FileFunction.cached(
          streams.value.cacheDirectory / s"sttp-openapi-src-$scalaVer",
          FileInfo.hash
        ) { input: Set[File] =>
          input.foldLeft(Set.empty[File]) { (result, swaggerPath) =>
            val swaggerYaml = IO.read(swaggerPath)
            val code = codegen.generateUnsafe(swaggerYaml)
            val targetFile = sourcesDir / "SttpOpenApi.scala"
            IO.write(targetFile, format(scalafmt, code.toString(), targetFile))
            result + targetFile
          }
        }
        cachedFun(Set(swaggerDir)).toSeq
      }
  }

  private def format(scalafmt: Scalafmt, code: String, futureFile: File) = {
    val scalafmtConfig = Paths.get(".scalafmt.conf")
    if (scalafmtConfig.toFile.exists()) {
      scalafmt.format(scalafmtConfig, futureFile.toPath, code)
    } else {
      code
    }
  }

  import autoImport._

  private lazy val coreDeps = List(
    "com.softwaremill.sttp.client3" %% "core" % "3.2.3"
  )

  private lazy val circeDeps = List(
    "io.circe" %% "circe-core" % "0.13.0",
    "io.circe" %% "circe-generic" % "0.13.0",
    "io.circe" %% "circe-parser" % "0.13.0",
    "com.softwaremill.sttp.client3" %% "circe" % "3.2.3"
  )

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      Compile / sourceGenerators += generateSources.taskValue,
      libraryDependencies ++= coreDeps ++ circeDeps
    )
}
