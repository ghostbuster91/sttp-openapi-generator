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
        .map(_.getSchemas.asScala.toMap)
        .getOrElse(Map.empty[String, Schema[_]])
    val enums =
      collectEnums(safeSchemaComponents, Nil, Nil).map(enumToSealedTraitDef)

    val modelClassNames = //TODO should be created based on model and enums?
      safeSchemaComponents.map { case (key, _) =>
        SchemaRef.fromKey(key) -> key
      }
    val model = safeSchemaComponents.map { case (key, schema: ObjectSchema) =>
      val schemaRef = SchemaRef.fromKey(key)
      schemaRef -> schemaToClassDef(
        modelClassNames(schemaRef),
        schema,
        modelClassNames
      )
    }
    val operations = collectOperations(openApi, model, modelClassNames)
    val tree =
      q"""package io.github.ghostbuster91.sttp.client3.example {

          import _root_.sttp.client3._
          import _root_.sttp.model._
          import _root_.sttp.client3.circe._
          import _root_.io.circe.generic.auto._
          import _root_.java.io.File

          ..${enums.map(_.st)}
          ..${enums.map(_.companion)}
          ..${model.values.toList}

          class Api(baseUrl: String) {
            ..$operations
          }
        }
      """
    tree.toString
  }

  private def collectEnums(
      schemas: Map[String, Schema[_]],
      acc: List[Enum],
      path: List[String]
  ): List[Enum] =
    schemas.flatMap { case (k, schema) =>
      Option(schema.getEnum)
        .map(_.asScala.toList)
        .map(e => Enum(path :+ k, e.map(_.asInstanceOf[String]))) ++
        Option(schema.getProperties())
          .map(_.asScala.toMap)
          .map(props => collectEnums(props, acc, path :+ k))
          .getOrElse(acc)
    }.toList

  private def collectOperations(
      openApi: OpenAPI,
      model: Map[SchemaRef, Defn.Class],
      modelClassNames: Map[SchemaRef, String]
  ) =
    openApi.getPaths.asScala.toList.flatMap { case (path, item) =>
      List(
        Option(item.getGet).map(op => op -> Method.Get),
        Option(item.getPut()).map(op => op -> Method.Put),
        Option(item.getPost()).map(op => op -> Method.Post)
      ).flatten.map { case (operation, method) =>
        val uri = constructUrl(
          path,
          Option(operation.getParameters).toList.flatMap(_.asScala.toList)
        )
        val request = createRequestCall(method, uri)
        processOperation(
          operation,
          model,
          request,
          modelClassNames
        )
      }
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

  private def createRequestCall(method: Method, uri: Term) =
    method match {
      case Method.Put    => q"basicRequest.put($uri)"
      case Method.Get    => q"basicRequest.get($uri)"
      case Method.Post   => q"basicRequest.post($uri)"
      case Method.Delete => q"basicRequest.delete($uri)"
      case Method.Head   => q"basicRequest.head($uri)"
      case Method.Patch  => q"basicRequest.patch($uri)"
    }

  private def processOperation(
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
                model(
                  SchemaRef(jsonResponse.getSchema().get$ref())
                ).name.value //TODO use schemaToClassName?
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
        val paramName = Term.Name(pathParam.getName)
        val paramType = optionApplication(
          schemaToType(
            "outerPathName",
            pathParam.getName,
            pathParam.getSchema,
            schemaRefToClassName
          ),
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
        val paramName = Term.Name(queryParam.getName)
        val paramType = optionApplication(
          schemaToType(
            "outerName",
            queryParam.getName,
            queryParam.getSchema,
            schemaRefToClassName
          ),
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

  private def enumToSealedTraitDef(enum: Enum) = {
    val name = enum.path.takeRight(2).map(_.capitalize).mkString
    val objs = //List.empty[Defn.Object]
      enum.values.map(n =>
        Defn.Object(
          List(Mod.Case()),
          Term.Name(n.capitalize),
          Template(
            early = Nil,
            inits = List(Init(Type.Name(name), Name.Anonymous(), List.empty)),
            self = Self(
              name = Name.Anonymous(),
              decltpe = None
            ),
            stats = Nil
          )
        )
      )

    EnumDef(
      q"sealed trait ${Type.Name(name)}",
      q"""object ${Term.Name(name)} {
          ..$objs
      }
      """
    )
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
              name,
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
      className: String,
      name: String,
      schema: Schema[_],
      schemaRefToClassName: Map[SchemaRef, String],
      isRequired: Boolean
  ): Term.Param = {
    val declType = schemaToType(className, name, schema, schemaRefToClassName)
    paramDeclFromType(name, optionApplication(declType, isRequired))
  }

  private def schemaToType(
      className: String,
      propertyName: String,
      schema: Schema[_],
      schemaRefToClassName: Map[SchemaRef, String]
  ): Type =
    schema match {
      case ss: StringSchema
          if Option(ss.getEnum).map(_.asScala.toList).exists(_.nonEmpty) =>
        Type.Name(
          s"${className.capitalize}${propertyName.capitalize}"
        )
      case _: StringSchema =>
        Type.Name("String")
      case _: IntegerSchema =>
        Type.Name("Int")
      case s: ArraySchema =>
        t"List[${schemaToType(className, propertyName, s.getItems, schemaRefToClassName)}]"
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

sealed trait Method
object Method {
  case object Put extends Method
  case object Get extends Method
  case object Post extends Method
  case object Delete extends Method
  case object Head extends Method
  case object Patch extends Method
  //TODO trace, connect, option?
}
case class Enum(path: List[String], values: List[String])
case class EnumDef(st: Defn.Trait, companion: Defn.Object)
