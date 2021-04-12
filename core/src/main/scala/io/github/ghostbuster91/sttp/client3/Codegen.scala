package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.json.circe._
import io.github.ghostbuster91.sttp.client3.openapi._

import io.github.ghostbuster91.sttp.client3.http._
import io.github.ghostbuster91.sttp.client3.model._
import scala.meta._

class Codegen(logger: LogAdapter, config: CodegenConfig) {
  def generateUnsafe(openApiYaml: String): Source = {
    val openApi = OpenApiLoader.load(openApiYaml)
    val schemas = openApi.components.map(_.schemas).getOrElse(Map.empty)
    val requestBodies = collectRequestBodies(openApi)
    val enums = EnumCollector.collectEnums(schemas)
    val ir = new ImportRegistry()
    ir.registerImport(q"import _root_.sttp.client3._")
    ir.registerImport(q"import _root_.sttp.model._")

    val jsonTypeProvider = new CirceTypeProvider(ir)
    val modelGenerator =
      ModelGenerator(schemas, requestBodies, ir, jsonTypeProvider)
    val model = modelGenerator.generate
    val operations = collectOperations(openApi)
    val processedOps =
      new ApiCallGenerator(modelGenerator, ir, config, jsonTypeProvider)
        .generate(operations)

    val coproducts = new CoproductCollector(modelGenerator, enums)
      .collect(schemas)
    val openProducts = new OpenProductCollector(modelGenerator).collect(schemas)
    val codecs = new CirceCodecGenerator(ir)
      .generate(enums, coproducts, openProducts)
      .stats

    createSource(
      processedOps,
      enums,
      ir.getImports,
      codecs,
      model.values.toList
    )
  }

  private def createSource(
      processedOps: Map[Option[String], List[Defn.Def]],
      enums: List[Enum],
      imports: List[Import],
      codecs: List[Stat],
      classes: List[Defn]
  ) = {
    val apiDefs = createApiDefs(processedOps)

    val enumDefs = enums.flatMap(EnumGenerator.enumToSealedTraitDef)
    source"""package io.github.ghostbuster91.sttp.client3.example {

          ..$imports

          ..$codecs

          ..$enumDefs
          ..$classes

          ..$apiDefs
        }
      """
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
          .get(MediaType.ApplicationJson.v)
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

case class CodegenConfig(handleErrors: Boolean)
