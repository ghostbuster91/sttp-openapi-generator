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
import scala.meta._
import _root_.io.swagger.v3.oas.models.Operation
import _root_.io.swagger.v3.oas.models.media.ArraySchema

case class SchemaRef(key: String)
object SchemaRef {
  def fromKey(key: String): SchemaRef =
    SchemaRef(s"#/components/schemas/$key")
}

object Generator {
  def generateUnsafe(openApiYaml: String): String = {
    val openApi = loadOpenApi(openApiYaml)
    val modelClassNames = openApi.getComponents.getSchemas.asScala.map {
      case (key, _) =>
        SchemaRef.fromKey(key) -> key
    }.toMap
    val model = openApi.getComponents.getSchemas.asScala.map {
      case (key, schema: ObjectSchema) =>
        val schemaRef = SchemaRef.fromKey(key)
        schemaRef -> schemaToClassDef(
          modelClassNames(schemaRef),
          schema,
          modelClassNames
        )
    }.toMap
    val ops = openApi.getPaths.asScala.toList.flatMap { case (path, item) =>
      List(
        Option(item.getGet).map(processGetOperation(_, model)),
        Option(item.getPut()).map(processPutOperation(_, model))
      ).flatten
    }
    val tree =
      q"""package io.github.ghostbuster91.sttp.client3.example {

          import _root_.sttp.client3._
          import _root_.sttp.model._
          import _root_.sttp.client3.circe._
          import _root_.io.circe.generic.auto._

          ..${model.values.toList.reverse}

          class Api(serverUrl: String) {
            ..$ops
          }
        }
      """
    val code = tree.toString
    code
  }

  private def processGetOperation(
      getOperation: Operation,
      model: Map[SchemaRef, Defn.Class]
  ) = {
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
    val variableName = Pat.Var(Term.Name(operationId))
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

  def processPutOperation(
      putOperation: Operation,
      model: Map[SchemaRef, Defn.Class]
  ) = {
    val operationId = putOperation.getOperationId
    val responseClassName = putOperation.getResponses.asScala
      .collectFirst { case ("200", response) =>
        response.getContent.asScala.collectFirst {
          case ("application/json", jsonResponse) =>
            model(SchemaRef(jsonResponse.getSchema().get$ref())).name.value
        }
      }
      .flatten
      .get

    val requestClassName =
      putOperation.getRequestBody
        .getContent()
        .asScala
        .collectFirst { case ("application/json", jsonRequest) =>
          model(SchemaRef(jsonRequest.getSchema().get$ref())).name.value
        }
        .get

    val paramName = Term.Name(s"a$requestClassName")
    val paramType = Type.Name(requestClassName)

    val parameter = param"$paramName : $paramType"

    val functionName = Term.Name(operationId)
    val responseClassType = Type.Name(responseClassName)
    q"""def $functionName($parameter) = basicRequest.put(
                    Uri.unsafeApply("https",
                        serverUrl,
                        Seq.empty,
                    )
                  )
                  .body($paramName)
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

  private def schemaToClassDef(
      name: String,
      schema: ObjectSchema,
      schemaRefToClassName: Map[SchemaRef, String]
  ) =
    Defn.Class(
      List(Mod.Case()),
      Type.Name(name),
      Nil,
      Ctor.Primary(
        Nil,
        Name(""),
        List(
          schema.getProperties.asScala.map { case (k, v) =>
            processParams(
              k,
              v,
              schemaRefToClassName,
              schema.getRequired().asScala.contains(k)
            )
          }.toList
        )
      ),
      Template(Nil, Nil, Self(Name(""), None), Nil)
    )

  private def processParams(
      name: String,
      schema: Schema[_],
      schemaRefToClassName: Map[SchemaRef, String],
      isRequired: Boolean
  ): Term.Param = {
    val declType = declTypeFromSchema(schema, schemaRefToClassName)
    paramDeclFromType(name, optionApplication(declType, isRequired))
  }

  private def declTypeFromSchema(
      schema: Schema[_],
      schemaRefToClassName: Map[SchemaRef, String]
  ): Type =
    schema match {
      case _: StringSchema =>
        Type.Name("String")
      case _: IntegerSchema =>
        Type.Name("Int")
      case s: ArraySchema =>
        t"List[${declTypeFromSchema(s.getItems(), schemaRefToClassName)}]"
      case s: Schema[_] =>
        Option(s.get$ref) match {
          case Some(value) =>
            Type.Name(schemaRefToClassName(SchemaRef(value)))
          case None =>
            throw new IllegalArgumentException(s"Schema without reference $s")
        }
    }

  private def optionApplication(declType: Type, isRequired: Boolean): Type =
    if (isRequired) {
      declType
    } else {
      t"Option[$declType]"
    }

  private def paramDeclFromType(paramName: String, declType: Type) =
    Term.Param(
      Nil,
      Term.Name(paramName),
      Some(declType),
      None
    )
}
