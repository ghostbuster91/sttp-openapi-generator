package io.github.ghostbuster91.sttp.client3

import sbt.{AutoPlugin, Def, File }
import sbt.Keys._
import sbt._

object SttpOpenApiCodegenPlugin extends AutoPlugin {
  object autoImport {
    lazy val generateSources =
      Def.task {
        val log = streams.value.log
        val codegen = new Codegen(new SbtLogAdapter(log))

        val sourcesDir = (Compile / sourceManaged).value
        val ver = scalaVersion.value
        val swaggerDir = (Compile / resourceDirectory).value / "swagger.yaml"
        val cachedFun = FileFunction.cached(
          streams.value.cacheDirectory / s"k8s-monocle-src-${ver}",
          FileInfo.hash
        ) { input: Set[File] =>
          input.foldLeft(Set.empty[File]) { (result, swaggerPath) =>
            val swaggerYaml = IO.readLines(swaggerPath).mkString("\n")
            val code = codegen.generateUnsafe(swaggerYaml)
            val targetFile = sourcesDir / "SttpOpenApi.scala"
            IO.write(targetFile, code.toString())
            result + targetFile
          }
        }
        cachedFun(Set(swaggerDir)).toSeq
      }
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      Compile / sourceGenerators += generateSources.taskValue
    )
}