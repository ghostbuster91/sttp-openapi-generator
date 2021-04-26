package io.github.ghostbuster91.sttp.client3

import org.scalafmt.interfaces.Scalafmt
import sbt._
import sbt.internal.util.ManagedLogger

import java.nio.file.Paths

class SbtCodegenAdapter(
    config: CodegenConfig,
    targetDirectory: File,
    topLevelInputPath: File,
    log: ManagedLogger,
    scalafmt: Scalafmt
) {

  private lazy val codegen = new Codegen(new SbtLogAdapter(log), config)

  def processSingleFile(
      inputFile: File
  ) = {
    log.info(s"Generating classes for ${inputFile.getAbsolutePath}...")
    val swaggerYaml = IO.read(inputFile)
    val relativePath = IO
      .relativizeFile(topLevelInputPath, inputFile)
      .getOrElse(
        throw new IllegalArgumentException(
          s"Given $inputFile is not a descendant of $topLevelInputPath"
        )
      )
      .getParent
    val code = codegen.generateUnsafe(
      swaggerYaml,
      relativePath
        .replace("/", ".")
    )
    val targetFile =
      targetDirectory / relativePath / s"${inputFile.base.capitalize}.scala"
    IO.write(targetFile, format(scalafmt, code.toString(), targetFile))
    targetFile
  }

  private def format(scalafmt: Scalafmt, code: String, futureFile: File) = {
    val scalafmtConfig = Paths.get(".scalafmt.conf")
    if (scalafmtConfig.toFile.exists()) {
      log.info(s"Formatting ${futureFile.getAbsolutePath}")
      scalafmt.format(scalafmtConfig, futureFile.toPath, code)
    } else {
      code
    }
  }
}
