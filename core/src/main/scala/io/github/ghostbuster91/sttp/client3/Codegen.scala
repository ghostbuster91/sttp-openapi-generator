package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.json.circe._
import io.github.ghostbuster91.sttp.client3.openapi._
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.api._

import scala.meta._
import sttp.model.MediaType
import sttp.model.Method

class Codegen(logger: LogAdapter, config: CodegenConfig) {
  def generateUnsafe(
      openApiYaml: String,
      packageName: Option[String]
  ): Source = {
    val openApi = new SafeOpenApiParser(logger).parse(openApiYaml)
    val schemas = openApi.components.map(_.schemas).getOrElse(Map.empty)
    val requestBodies = collectRequestBodies(openApi)
    val enums = EnumCollector.collectEnums(schemas)
    val InitialImports = ImportRegistry(
      q"import _root_.sttp.client3._",
      q"import _root_.sttp.model._"
    )

    val jsonTypeProvider = CirceTypeProvider
    val model = Model(schemas, requestBodies)
    val coproducts = new CoproductCollector(model, enums).collect(schemas)
    val modelGenerator = ModelGenerator(model, jsonTypeProvider)
    val operations = collectOperations(openApi)
    val (imports, output) = (for {
      classes <- modelGenerator.generate
      apiCalls <- new ApiCallGenerator(model, config, jsonTypeProvider)
        .generate(operations)
      openProducts <- new OpenProductCollector(model).collect(schemas)
      codecs <- new CirceCodecGenerator().generate(
        enums,
        coproducts,
        openProducts
      )
    } yield CodegenOutput(
      apiCalls,
      enums,
      InitialImports.getImports,
      codecs.stats,
      classes.values.toList
    )).run(InitialImports).value

    createSource(imports.getImports, output, packageName)
  }

  private def createSource(
      imports: List[Import],
      codegenOutput: CodegenOutput,
      rawPkgName: Option[String]
  ): Source = {
    val apiDefs = createApiDefs(codegenOutput.processedOps)
    val enumDefs = codegenOutput.enums
      .flatMap(EnumGenerator.enumToSealedTraitDef)
    val pkgName = rawPkgName.map(
      _.parse[Term].get
        .asInstanceOf[Term.Ref]
    )
    val src = source"""
          ..$imports

          ..${codegenOutput.codecs}

          ..$enumDefs
          ..${codegenOutput.classes}

          ..$apiDefs
      """
    pkgName match {
      case Some(value) =>
        source"""package $value {
                ..${src.stats}
              }"""
      case None =>
        logger.warn(
          "Generating code without package. Consider putting your openapi definition into a sub-directory."
        )
        src
    }
  }

  private def createApiDefs(processedOps: Map[Option[String], List[Defn.Def]]) =
    processedOps.map { case (key, apiCalls) =>
      val className =
        Type.Name(key.map(_.capitalize).getOrElse("Default") + "Api")
      q"""class $className(baseUrl: String) extends CirceCodecs {
            ..$apiCalls
          }
      """
    }.toList

  private def collectRequestBodies(openApi: SafeOpenApi) =
    openApi.components
      .map(_.requestBodies)
      .getOrElse(Map.empty)
      .flatMap { case (k, rb) =>
        rb.content
          .get(MediaType.ApplicationJson.toString)
          .map(mt => k -> mt.schema)
      }
      .toMap

  private def collectOperations(openApi: SafeOpenApi) =
    openApi.paths.toList.flatMap { case (path, item) =>
      item.operations.map { case (method, operation) =>
        CollectedOperation(path, method, operation)
      }
    }
}

case class CollectedOperation(
    path: String,
    method: Method,
    operation: SafeOperation
)

case class CodegenConfig(
    handleErrors: Boolean,
    jsonLibrary: JsonLibrary
)
case class CodegenOutput(
    processedOps: Map[Option[String], List[Defn.Def]],
    enums: List[Enum],
    imports: List[Import],
    codecs: List[Stat],
    classes: List[Defn]
)

sealed trait JsonLibrary
object JsonLibrary {
  object Circe extends JsonLibrary
}
