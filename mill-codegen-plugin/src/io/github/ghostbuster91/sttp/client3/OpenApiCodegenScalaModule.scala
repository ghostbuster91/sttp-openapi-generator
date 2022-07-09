package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.OpenApiCodegenScalaModule.*
import mill.*
import mill.api.Result
import mill.scalalib.*
import os.{Path, RelPath}
import upickle.default.*

trait OpenApiCodegenScalaModule extends ScalaModule {

  /** Input resources for sttp-openapi generator
    */
  def sttpOpenApiInput: T[Seq[Input]] = T {
    Seq(
      Input.dir(millSourcePath / "src" / "main" / "openapi"),
      Input.dir(millSourcePath / "openapi")
    )
  }

  /** Json library for sttp-openapi generator to use
    */
  def sttpOpenApiJsonLibrary: T[JsonLibrary] = T(JsonLibrary.Circe: JsonLibrary)

  /** If true the generator will include error information in types
    */
  def sttpOpenApiHandleErrors: T[Boolean] = T(true)

  /** If true the generator will render model classes only if they are
    * referenced by any of the exiting operations
    */
  def sttpOpenApiMinimizeOutput: T[Boolean] = T(true)

  /** Configuration settings for sttp-openapi generator to use
    */
  def sttpOpenApiTypesMapping: T[TypesMapping] = T(TypesMapping())

  override def ivyDeps = T {
    super.ivyDeps() ++ coreDeps ++ circeDeps
  }

  private lazy val coreDeps =
    Agg(ivy"com.softwaremill.sttp.client3::core::3.3.18")

  private lazy val circeDeps = Agg(
    ivy"io.circe::circe-core::0.14.2",
    ivy"io.circe::circe-parser::0.14.2",
    ivy"com.softwaremill.sttp.client3::circe::3.3.18"
  )

  override def generatedSources = T {
    super.generatedSources() :+ compileOpenApiSpec()
  }

  def compileOpenApiSpec: T[PathRef] = T {
    val outDir = T.dest
    val listOfResults = collectInputs(sttpOpenApiInput()).map {
      case (path, opts) =>
        println(
          s"[SttpOpenapi] Generating classes for ${path.toIO.getAbsolutePath}..."
        )
        val config = CodegenConfig(
          handleErrors = sttpOpenApiHandleErrors(),
          sttpOpenApiJsonLibrary(),
          sttpOpenApiMinimizeOutput(),
          sttpOpenApiTypesMapping()
        )
        val input = os.read.lines(path).mkString("\n")
        val result =
          new Codegen(LogAdapter.StdOut, config).generate(input, Some(opts.pkg))
        result
          .map { source =>
            val targetFileName = snakeToCamelCase(
              opts.relPath.segments.last
                .replace(".yaml", ".scala")
                .replace(".yml", ".scala")
            )
            val targetDirectory = outDir / opts.relPath / RelPath.up
            targetDirectory.toIO.getName
            os.write.over(
              target = targetDirectory / targetFileName,
              data = source.toString(),
              createFolders = true
            )
            println(s"Generated ${targetDirectory / targetFileName}")
          }
          .left
          .map { errs =>
            s"Processing of $path failed. Error: ${errs.mkString("\n")}"
          }
    }
    val combinedResults =
      listOfResults.foldLeft(Right(()): Either[List[String], Unit]) {
        (acc, item) =>
          (acc, item) match {
            case (Left(errs), Right(_))  => Left(errs)
            case (Left(errs), Left(err)) => Left(errs :+ err)
            case (Right(_), Right(_))    => Right(())
            case (Right(_), Left(err))   => Left(List(err))
          }
      }
    combinedResults match {
      case Left(value) => Result.Failure(value.mkString("\n"))
      case Right(_)    => Result.Success(PathRef(outDir))
    }
  }

  private def collectInputs(
      inputFiles: Seq[Input]
  ): List[(Path, FileOpts)] =
    inputFiles.toList.flatMap {
      case Input.SingleFile(f, pkg) =>
        if (f.toIO.exists()) {
          List((f, FileOpts(pkg = pkg, RelPath.rel)))
        } else {
          println(
            s"[SttpOpenapi] Input directory $f does not exist. Skipping generation..."
          )
          List.empty
        }
      case Input.Directory(topLevelDir, basePkg) =>
        collectInputFiles(topLevelDir).map(file =>
          relativePathToPackage(topLevelDir, file, basePkg)
        )
    }

  private def relativePathToPackage(
      topLevel: Path,
      file: Path,
      basePkg: Option[String]
  ) = {
    val relativePath = file.relativeTo(
      topLevel
    ) //TODO handle case where file is not within topLevel directory
    val calculatedPkg = relativePath.segments.dropRight(1).mkString(".")
    val fullPkg = (basePkg, calculatedPkg) match {
      case (Some(b), p) => s"$b.$p"
      case (None, p)    => p
    }
    (file, FileOpts(pkg = fullPkg, relativePath))
  }

  private def collectInputFiles(f: Path) =
    if (f.toIO.exists()) {
      os.walk(f)
        .filter(p =>
          p.toIO.isFile && (p.toIO.getName.endsWith(".yaml") || p.toIO.getName
            .endsWith(".yml"))
        )
    } else {
      println(
        s"[SttpOpenapi] Input directory $f does not exist. Skipping generation..."
      )
      List.empty
    }

  private def snakeToCamelCase(snake: String) =
    snake.split('_').toList.map(_.capitalize).mkString
}

object OpenApiCodegenScalaModule {

  sealed trait Input
  object Input {
    case class SingleFile(file: Path, pkg: String) extends Input
    object SingleFile {
      implicit val rw: ReadWriter[SingleFile] = macroRW
    }
    case class Directory(directory: Path, basePkg: Option[String]) extends Input
    object Directory {
      implicit val rw: ReadWriter[Directory] = macroRW
    }

    def dir(path: Path, basePkg: Option[String] = None): Input =
      Input.Directory(path, basePkg)

    implicit val rw: ReadWriter[Input] =
      ReadWriter.merge(SingleFile.rw, Directory.rw)
  }

  implicit def relPathRw: ReadWriter[RelPath] =
    implicitly[ReadWriter[IndexedSeq[String]]]
      .bimap[RelPath](
        v => v.segments,
        g => RelPath(g, 0)
      )

  case class FileOpts(pkg: String, relPath: RelPath)
  object FileOpts {
    implicit val rw: ReadWriter[FileOpts] = macroRW
  }

//  implicit def classRw: ReadWriter[Class[Any]] = implicitly[ReadWriter[String]]
//    .bimap[Class[Any]](
//      v => v.getName,
//      g => Class.forName(g).asInstanceOf[Class[Any]]
//    )

  implicit val typesMappingRw: ReadWriter[TypesMapping] =
    implicitly[ReadWriter[Map[String, String]]].bimap[TypesMapping](
      v => Map("dateTime" -> v.dateTime.getName),
      g => TypesMapping(dateTime = Class.forName(g("dateTime")))
    )

  implicit val jsonLibraryCirceRw: ReadWriter[JsonLibrary.Circe.type] = macroRW
  implicit val jsonLibraryRw: ReadWriter[JsonLibrary] =
    ReadWriter.merge(jsonLibraryCirceRw)
}
