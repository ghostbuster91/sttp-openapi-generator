package io.github.ghostbuster91.sttp.client3

import circe._

import io.github.ghostbuster91.sttp.client3.http._
import scala.meta._

class Codegen(logger: LogAdapter) {
  def generateUnsafe(openApiYaml: String): Source = {
    val openApi = OpenApiLoader.load(openApiYaml)
    val schemas = openApi.components.map(_.schemas).getOrElse(Map.empty)
    val requestBodies =
      openApi.components
        .map(_.requestBodies)
        .getOrElse(Map.empty)
        .flatMap { case (k, rb) =>
          rb.content
            .get(MediaType.ApplicationJson.v)
            .map(mt => k -> mt.schema)
        }
        .toMap
    val enums = EnumCollector.collectEnums(schemas)
    val enumDefs = enums.flatMap(EnumGenerator.enumToSealedTraitDef)
    val ir = new ImportRegistry()
    ir.registerImport(q"import _root_.sttp.client3._")
    ir.registerImport(q"import _root_.sttp.model._")

    val modelGenerator =
      ModelGenerator(schemas, requestBodies, ir, new CirceTypeProvider(ir))
    val model = modelGenerator.generate
    val operations = collectOperations(openApi)
    val processedOps =
      new ApiCallGenerator(modelGenerator, ir).generate(operations)

    val apiDefs = processedOps.map { case (key, apiCalls) =>
      val className =
        Type.Name(key.map(_.capitalize).getOrElse("Default") + "Api")
      q"""class $className(baseUrl: String) extends CirceCodecs {
            ..$apiCalls
          }
      """
    }.toList
    val coproducts =
      new CoproductCollector(modelGenerator, enums).collect(schemas)
    val openProducts = new OpenProductCollector(modelGenerator).collect(schemas)
    val codecs = new CirceCodecGenerator(ir)
      .generate(enums, coproducts, openProducts)
      .stats

    source"""package io.github.ghostbuster91.sttp.client3.example {

          ..${ir.getImports}

          ..$codecs

          ..$enumDefs
          ..${model.values.toList}

          ..$apiDefs
        }
      """
  }

  private def collectOperations(
      openApi: SafeOpenApi
  ) =
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
