package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.json.circe._
import io.github.ghostbuster91.sttp.client3.openapi._
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.api._

import scala.meta._
import sttp.model.MediaType
import sttp.model.Method

import java.time.{LocalDate, LocalDateTime}
import _root_.io.github.ghostbuster91.sttp.client3.openapi.zz.OpenapiModels._

class Codegen(logger: LogAdapter, config: CodegenConfig) {
  def generateUnsafe(openApiYaml: String, packageName: Option[String]): Source =
    generate(openApiYaml, packageName) match {
      case Left(errors) => throw new RuntimeException(errors.toString())
      case Right(value) => value
    }

  def generate(
      openApiYaml: String,
      packageName: Option[String]
  ): Either[Seq[String], Source] =
    YamlParser
      .parseFile(openApiYaml)
      .map(openApi => generate(packageName, openApi))

  private def generate(
      packageName: Option[String],
      openApi: OpenapiDocument
  ) = {
    val schemas = openApi.components.map(_.schemas).getOrElse(Map.empty)
    val requestBodies = collectRequestBodies(openApi)
    val enums = EnumCollector.collectEnums(schemas)
    val InitialImports = ImportRegistry(
      q"import _root_.sttp.client3._",
      q"import _root_.sttp.model._"
    )

    val jsonTypeProvider = CirceTypeProvider
    val operations = collectOperations(openApi)
    val model =
      createModel(schemas, requestBodies, operations, config.typesMapping)
    val (imports, output) = (for {
      apiCalls <- new ApiCallGenerator(model, config, jsonTypeProvider)
        .generate(operations)
      coproducts <- new CoproductCollector(
        model,
        enums,
        jsonTypeProvider
      )
        .collect(model.schemas)
      products <- new ProductCollector(model, jsonTypeProvider)
        .collect(
          model.schemas
        )
      classes = ModelGenerator.generate(coproducts, products)
      codecs <- new CirceCodecGenerator().generate(
        enums,
        coproducts,
        products
      )
    } yield CodegenOutput(
      apiCalls,
      enums,
      InitialImports.getImports,
      codecs.stats,
      classes
    )).run(InitialImports).value

    createSource(imports.getImports, output, packageName)
  }

  private def createModel(
      schemas: Map[String, SafeSchema],
      requestBodies: Map[String, SafeSchema],
      operations: List[CollectedOperation],
      typeMappings: TypesMapping
  ) = {
    val model = Model(schemas, requestBodies, typeMappings)
    if (config.minimize) {
      val usedReferences =
        new ReferenceCollector(model).collect(operations.map(_.operation))
      model.copy(schemas =
        model.schemas.filterKeys(usedReferences.contains).toMap
      )
    } else {
      model
    }
  }

  private def createSource(
      imports: List[Import],
      codegenOutput: CodegenOutput,
      rawPkgName: Option[String]
  ): Source = {
    val apiDefs = createApiDefs(codegenOutput.processedOps)
    val enumDefs = codegenOutput.enums
      .sortBy(_.name)
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
      q"""class $className(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
            import circeCodecs._
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
    handleErrors: Boolean = false,
    jsonLibrary: JsonLibrary = JsonLibrary.Circe,
    minimize: Boolean = true,
    typesMapping: TypesMapping = TypesMapping()
)

case class TypesMapping(
    dateTime: Class[_] = classOf[LocalDateTime],
    date: Class[_] = classOf[LocalDate]
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
