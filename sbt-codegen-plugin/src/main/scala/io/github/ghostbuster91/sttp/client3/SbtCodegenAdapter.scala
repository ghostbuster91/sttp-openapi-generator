package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.SbtCodegenAdapter.FileOpts
import org.scalafmt.interfaces.Scalafmt
import sbt.internal.util.ManagedLogger
import sbt._

import java.nio.file.Paths

class SbtCodegenAdapter(
    config: CodegenConfig,
    targetDirectory: File,
    log: ManagedLogger,
    scalafmt: Scalafmt
) {

  private lazy val codegen = new Codegen(new SbtLogAdapter(log), config)

  def processSingleFile(
      inputFile: File,
      opts: FileOpts
  ): Either[String, File] = {
    log.info(
      s"[SttpOpenapi] Generating classes for ${inputFile.getAbsolutePath}..."
    )
    val swaggerYaml = IO.read(inputFile)
    codegen
      .generate(
        swaggerYaml,
        opts.pkg
      )
      .map { code =>
        val targetFile =
          targetDirectory / opts.pkg
            .map(_.replace(".", "/"))
            .getOrElse(
              "."
            ) / s"${snakeToCamelCase(inputFile.base)}.scala"
        IO.write(targetFile, format(scalafmt, code.toString(), targetFile))
        targetFile
      }
  }

  private def format(scalafmt: Scalafmt, code: String, futureFile: File) = {
    val scalafmtConfig = Paths.get(".scalafmt.conf")
    if (scalafmtConfig.toFile.exists()) {
      log.info(s"[SttpOpenapi] Formatting ${futureFile.getAbsolutePath}")
      scalafmt.format(scalafmtConfig, futureFile.toPath, code)
    } else {
      code
    }
  }

  private def snakeToCamelCase(snake: String) =
    snake.split('_').toList.map(_.capitalize).mkString
}
object SbtCodegenAdapter {
  case class FileOpts(pkg: Option[String])
}
