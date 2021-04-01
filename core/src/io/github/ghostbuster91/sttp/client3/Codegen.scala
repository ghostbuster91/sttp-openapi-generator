package io.github.ghostbuster91.sttp.client3

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import scala.collection.JavaConverters._
import io.swagger.v3.parser.core.models.AuthorizationValue
import scala.meta._

object Codegen {
  def generateUnsafe(openApiYaml: String): Source = {
    val openApi = loadOpenApi(openApiYaml)
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
    val enums = EnumGenerator.generate(schemas)
    val ir = new ImportRegistry()
    ir.registerImport(q"import _root_.sttp.client3._")
    ir.registerImport(q"import _root_.sttp.model._")
    ir.registerImport(q"import _root_.sttp.client3.circe._")
    ir.registerImport(q"import _root_.io.circe.generic.auto._")

    val modelGenerator = ModelGenerator(schemas, requestBodies, ir)
    val model = modelGenerator.generate
    val operations = collectOperations(openApi)
    val processedOps =
      new ApiCallGenerator(modelGenerator, ir).generate(operations)

    val apiDefs = processedOps.map { case (key, apiCalls) =>
      val className =
        Type.Name(key.map(_.capitalize).getOrElse("Default") + "Api")
      q"""class $className(baseUrl: String) {
            ..$apiCalls
          }
      """
    }.toList

    source"""package io.github.ghostbuster91.sttp.client3.example {

          ..${ir.getImports}

          ..$enums
          ..${model.values.toList}

          ..$apiDefs
        }
      """
  }

  private def collectOperations(
      openApi: SafeOpenApi,
  ) =
    openApi.paths.toList.flatMap { case (path, item) =>
      item.operations.map { case (method, operation) =>
        CollectedOperation(path, method, operation)
      }
    }

  private def loadOpenApi(yaml: String): SafeOpenApi = {
    val parser = new OpenAPIParser
    val opts = new ParseOptions()
    opts.setResolve(true)
    opts.setFlatten(true)
    val parserResult = parser.readContents(
      yaml,
      List.empty[AuthorizationValue].asJava,
      opts,
    )
    Option(parserResult.getMessages).foreach { messages =>
      messages.asScala.foreach(println)
    }
    Option(parserResult.getOpenAPI) match {
      case Some(spec) =>
        new SafeOpenApi(spec)
      case None =>
        throw new RuntimeException(s"Failed to parse k8s swagger specs")
    }
  }
}

case class Enum(
    path: List[String],
    values: List[EnumValue],
    enumType: EnumType,
) {
  def name: String = path.takeRight(2).map(_.capitalize).mkString
  def uncapitalizedName: String =
    name.take(1).toLowerCase() + name.drop(1)
}
case class EnumValue(rawValue: Any) {
  def name: String = rawValue.toString.capitalize
}
sealed trait EnumType
object EnumType {
  case object EString extends EnumType
  case object EInt extends EnumType
}

case class CollectedOperation(
    path: String,
    method: Method,
    operation: SafeOperation,
)
