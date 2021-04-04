package io.github.ghostbuster91.sttp.client3

import circe._
import scala.meta._

object Codegen {
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
    val enums = EnumCollector.collectEnums(schemas, Nil)
    val enumDefs = enums.flatMap(EnumGenerator.enumToSealedTraitDef)
    val ir = new ImportRegistry()
    ir.registerImport(q"import _root_.sttp.client3._")
    ir.registerImport(q"import _root_.sttp.model._")

    val modelGenerator = ModelGenerator(schemas, requestBodies, ir)
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
    val codecs = new CirceCodecGenerator(ir).generate(enums, coproducts).stats

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

case class CodegenOutput(sources: Map[CodegenSourceType, Source])
sealed trait CodegenSourceType
object CodegenSourceType {
  case object Api extends CodegenSourceType
  case object Codecs extends CodegenSourceType
}

sealed trait Enum {
  def path: String
  def values: List[EnumValue]

  def name: String = path.capitalize
  def uncapitalizedName: String = name.take(1).toLowerCase() + name.drop(1)
}
object Enum {
  case class StringEnum(path: String, values: List[EnumValue.StringEv])
      extends Enum
  case class IntEnum(path: String, values: List[EnumValue.IntEv]) extends Enum
}

sealed trait EnumValue {
  def fqnName(enum: Enum): Term
  def simpleName: Term.Name
}
object EnumValue {
  case class StringEv(v: String) extends EnumValue {
    override def fqnName(enum: Enum): Term =
      q"${Term.Name(enum.name)}.${Term.Name(v.capitalize)}"
    override def simpleName: Term.Name = Term.Name(v.capitalize)
  }
  case class IntEv(v: Int) extends EnumValue {
    override def fqnName(enum: Enum): Term =
      q"${Term.Name(enum.name)}.${Term.Name(v.toString)}"
    override def simpleName: Term.Name = Term.Name(v.toString)
  }
}
