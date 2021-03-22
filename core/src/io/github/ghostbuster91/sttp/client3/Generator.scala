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
import _root_.io.swagger.v3.oas.models.parameters.Parameter
import _root_.io.swagger.v3.oas.models.parameters.QueryParameter
import scala.collection.immutable
import _root_.io.swagger.v3.oas.models.parameters.PathParameter

case class SchemaRef(key: String)
object SchemaRef {
  def fromKey(key: String): SchemaRef =
    SchemaRef(s"#/components/schemas/$key")
}

object Generator {
  def generateUnsafe(openApiYaml: String): String = {
    val openApi = loadOpenApi(openApiYaml)
    val safeSchemaComponents =
      Option(openApi.getComponents)
        .map(_.getSchemas.asScala)
        .getOrElse(Map.empty[String, Schema[_]])
    val modelClassNames =
      safeSchemaComponents.map { case (key, _) =>
        SchemaRef.fromKey(key) -> key
      }.toMap
    val model = safeSchemaComponents.map { case (key, schema: ObjectSchema) =>
      val schemaRef = SchemaRef.fromKey(key)
      schemaRef -> schemaToClassDef(
        modelClassNames(schemaRef),
        schema,
        modelClassNames
      )
    }.toMap
    val ops = openApi.getPaths.asScala.toList.flatMap { case (path, item) =>
      List(
        Option(item.getGet).map(
          processGetOperation(_, path, model, modelClassNames)
        ),
        Option(item.getPut())
          .map(processPutOperation(_, path, model, modelClassNames)),
        Option(item.getPost()).map(
          processPostOperation(_, path, model, modelClassNames)
        )
      ).flatten
    }
    val tree =
      q"""package io.github.ghostbuster91.sttp.client3.example {

          import _root_.sttp.client3._
          import _root_.sttp.model._
          import _root_.sttp.client3.circe._
          import _root_.io.circe.generic.auto._
          import _root_.java.io.File

          ..${model.values.toList.reverse}

          class Api(baseUrl: String) {
            ..$ops
          }
        }
      """
    val code = tree.toString
    code
  }

  private def constructUrl(path: String, params: List[Parameter]) = {
    val pathList =
      path
        .split("\\{[^/]*\\}")
        .toList
        .dropWhile(_ == "/")
    val queryParams = params.collect { case q: QueryParameter => q.getName() }
    val querySegments = queryParams
      .foldLeft(List.empty[String]) { (acc, item) =>
        acc match {
          case list if list.nonEmpty => list :+ s"&$item="
          case immutable.Nil         => List(s"?$item=")
        }
      }

    val pathParams = params.collect { case p: PathParameter =>
      Term.Name(p.getName())
    }
    val pathAndQuery = (pathList.dropRight(1) ++ List(
      pathList.lastOption.getOrElse("") ++ querySegments.headOption.getOrElse(
        ""
      )
    ) ++ querySegments.drop(1))
      .map(Lit.String(_))
    val pathAndQueryAdjusted = if (pathList.isEmpty && querySegments.isEmpty) {
      List(Lit.String(""))
    } else if (pathParams.isEmpty && queryParams.nonEmpty) {
      querySegments.map(Lit.String(_)) :+ Lit.String("")
    } else if (querySegments.isEmpty && pathParams.nonEmpty) {
      pathList.map(Lit.String(_)) :+ Lit.String("")
    } else if (querySegments.isEmpty && pathList.nonEmpty) {
      pathList.map(Lit.String(_))
    } else {
      pathAndQuery :+ Lit.String("")
    }

    Term.Interpolate(
      Term.Name("uri"),
      List(Lit.String("")) ++ pathAndQueryAdjusted,
      List(Term.Name("baseUrl")) ++ pathParams ++ queryParams.map(Term.Name(_))
    )
  }

  private def processGetOperation(
      operation: Operation,
      path: String,
      model: Map[SchemaRef, Defn.Class],
      schemaRefToClassName: Map[SchemaRef, String]
  ) = {
    val uri = constructUrl(
      path,
      Option(operation.getParameters).toList.flatMap(_.asScala.toList)
    )
    val basicRequestWithMethod = q"basicRequest.get($uri)"
    processOperation(
      operation,
      model,
      basicRequestWithMethod,
      schemaRefToClassName
    )
  }

  private def processPutOperation(
      operation: Operation,
      path: String,
      model: Map[SchemaRef, Defn.Class],
      schemaRefToClassName: Map[SchemaRef, String]
  ) = {
    val uri = constructUrl(
      path,
      Option(operation.getParameters).toList.flatMap(_.asScala.toList)
    )
    val basicRequestWithMethod = q"basicRequest.put($uri)"
    processOperation(
      operation,
      model,
      basicRequestWithMethod,
      schemaRefToClassName
    )
  }

  private def processPostOperation(
      operation: Operation,
      path: String,
      model: Map[SchemaRef, Defn.Class],
      schemaRefToClassName: Map[SchemaRef, String]
  ) = {
    val uri = constructUrl(
      path,
      Option(operation.getParameters).toList.flatMap(_.asScala.toList)
    )
    val basicRequestWithMethod = q"basicRequest.post($uri)"
    processOperation(
      operation,
      model,
      basicRequestWithMethod,
      schemaRefToClassName
    )
  }

  def processOperation(
      operation: Operation,
      model: Map[SchemaRef, Defn.Class],
      basicRequestWithMethod: Term,
      schemaRefToClassName: Map[SchemaRef, String]
  ) = {
    val operationId = operation.getOperationId
    val responseClassType = operation.getResponses.asScala.collectFirst {
      case ("200", response) =>
        Option(response.getContent)
          .flatMap(_.asScala.collectFirst {
            case ("application/json", jsonResponse) =>
              Type.Name(
                model(SchemaRef(jsonResponse.getSchema().get$ref())).name.value
              )
          })
          .getOrElse(Type.Name("Unit"))
    }.head
    val functionName = Term.Name(operationId)
    val queryParameters = queryParameter(operation, schemaRefToClassName)
    val pathParameters = pathParameter(operation, schemaRefToClassName)
    val bodyParameter = requestBodyParameter(operation, model)
    val parameters = pathParameters ++ queryParameters ++ bodyParameter
    val body: Term = Term.Apply(
      Term.Select(
        bodyParameter
          .map(p =>
            Term.Apply(
              Term.Select(basicRequestWithMethod, Term.Name("body")),
              List(Term.Name(p.name.value))
            )
          )
          .getOrElse(basicRequestWithMethod),
        Term.Name("response")
      ),
      List(q"asJson[$responseClassType].getRight")
    )
    q"def $functionName(..$parameters): Request[$responseClassType, Any] = $body"
  }

  private def pathParameter(
      operation: Operation,
      schemaRefToClassName: Map[SchemaRef, String]
  ) =
    Option(operation.getParameters).toList
      .flatMap(_.asScala.toList)
      .collect { case pathParam: PathParameter =>
        val paramName = Term.Name(pathParam.getName())
        val paramType = optionApplication(
          schemaToType(pathParam.getSchema(), schemaRefToClassName),
          pathParam.getRequired()
        )
        param"$paramName : $paramType"
      }

  private def queryParameter(
      operation: Operation,
      schemaRefToClassName: Map[SchemaRef, String]
  ) =
    Option(operation.getParameters).toList
      .flatMap(_.asScala.toList)
      .collect { case queryParam: QueryParameter =>
        val paramName = Term.Name(queryParam.getName())
        val paramType = optionApplication(
          schemaToType(queryParam.getSchema(), schemaRefToClassName),
          queryParam.getRequired()
        )
        param"$paramName : $paramType"
      }

  private def requestBodyParameter(
      operation: Operation,
      model: Map[SchemaRef, Defn.Class]
  ) =
    Option(operation.getRequestBody)
      .flatMap { requestBody =>
        val content = requestBody.getContent().asScala
        content
          .collectFirst {
            case ("application/json", jsonRequest) =>
              model(SchemaRef(jsonRequest.getSchema().get$ref())).name.value
            case ("application/octet-stream", _) =>
              "File"
          }
          .map { requestClassName =>
            val paramName = Term.Name(s"a$requestClassName")
            val paramType = optionApplication(
              Type.Name(requestClassName),
              requestBody.getRequired()
            )
            param"$paramName : $paramType"
          }
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
              schema.getRequired.asScala.contains(k)
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
    val declType = schemaToType(schema, schemaRefToClassName)
    paramDeclFromType(name, optionApplication(declType, isRequired))
  }

  private def schemaToType(
      schema: Schema[_],
      schemaRefToClassName: Map[SchemaRef, String]
  ): Type =
    schema match {
      case _: StringSchema =>
        Type.Name("String")
      case _: IntegerSchema =>
        Type.Name("Int")
      case s: ArraySchema =>
        t"List[${schemaToType(s.getItems(), schemaRefToClassName)}]"
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
