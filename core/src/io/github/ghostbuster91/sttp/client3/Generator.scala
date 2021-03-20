package io.github.ghostbuster91.sttp.client3

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.core.models.ParseOptions
import scala.collection.JavaConverters._
import io.swagger.v3.parser.core.models.AuthorizationValue
import io.swagger.v3.oas.models.media.ObjectSchema
import io.swagger.v3.oas.models.media.StringSchema
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.media.IntegerSchema
import io.swagger.v3.oas.models.PathItem
import scala.meta._

case class SchemaRef(key: String)

object Generator {
  def generateUnsafe(openApiYaml: String): String = {
    val openApi = loadOpenApi(openApiYaml)
    val model = openApi.getComponents.getSchemas.asScala.map {
      case (key, schema: ObjectSchema) =>
        SchemaRef(s"#/components/schemas/$key") -> schemaToClassDef(key, schema)
    }.toMap
    val ops = openApi.getPaths.asScala
      .map { case (path, item) =>
        processGetOperation(item, model)
      }
      .toList
      .flatten
    val tree =
      q"""package io.github.ghostbuster91.sttp.client3.example {

          import _root_.sttp.client3._
          import _root_.sttp.model._
          import _root_.sttp.client3.circe._
          import _root_.io.circe.generic.auto._

          ..${model.values.toList}

          class Api(serverUrl: String) {
            ..$ops
          }
        }
      """
    val code = tree.toString
    code
  }

  private def processGetOperation(
      item: PathItem,
      model: Map[SchemaRef, Defn.Class]
  ) =
    Option(item.getGet()).map { getOperation =>
      val operationId = getOperation.getOperationId
      val responseClassName = getOperation.getResponses.asScala
        .collectFirst { case ("200", response) =>
          response.getContent.asScala.collectFirst {
            case ("application/json", jsonResponse) =>
              model(SchemaRef(jsonResponse.getSchema().get$ref())).name.value
          }
        }
        .flatten
        .get
      val variableName = Pat.Var(Term.Name(s"get$responseClassName"))
      val responseClassType = Type.Name(responseClassName)
      q"""val $variableName = basicRequest.get(
                    Uri.unsafeApply("https",
                        serverUrl,
                        Seq.empty,
                    )
                )
                  .response(asJson[$responseClassType])
          """
    }

  private def loadOpenApi(yaml: String): OpenAPI = {
    val parser = new OpenAPIParser
    val opts = new ParseOptions()
    opts.setResolve(true)
    val parserResult = parser.readContents(
      yaml,
      List.empty[AuthorizationValue].asJava,
      opts
    )
    Option(parserResult.getMessages).foreach { messages =>
      messages.asScala.foreach(println)
    }
    Option(parserResult.getOpenAPI) match {
      case Some(spec) => spec
      case None =>
        throw new RuntimeException(s"Failed to parse k8s swagger specs")
    }
  }

  private def schemaToClassDef(name: String, schema: ObjectSchema) =
    Defn.Class(
      List(Mod.Case()),
      Type.Name(name),
      Nil,
      Ctor.Primary(
        Nil,
        Name(""),
        List(
          schema.getProperties.asScala.map { case (k, v) =>
            processParams(k, v)
          }.toList
        )
      ),
      Template(Nil, Nil, Self(Name(""), None), Nil)
    )

  private def processParams(name: String, schema: Schema[_]): Term.Param =
    schema match {
      case _: StringSchema =>
        Term.Param(
          Nil,
          Term.Name(name),
          Some(Type.Name("String")),
          None
        )
      case _: IntegerSchema =>
        Term.Param(
          Nil,
          Term.Name(name),
          Some(Type.Name("Int")),
          None
        )
    }

}
