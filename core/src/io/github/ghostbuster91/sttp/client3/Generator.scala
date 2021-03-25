package io.github.ghostbuster91.sttp.client3

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import scala.collection.JavaConverters._
import io.swagger.v3.parser.core.models.AuthorizationValue
import scala.meta._
import scala.collection.immutable

case class SchemaRef(key: String)
object SchemaRef {
  def fromKey(key: String): SchemaRef =
    SchemaRef(s"#/components/schemas/$key")
}

object Generator {
  def generateUnsafe(openApiYaml: String): String = {
    val openApi = loadOpenApi(openApiYaml)
    val safeSchemaComponents =
      openApi.components.map(_.schemas).getOrElse(Map.empty)
    val enums =
      collectEnums(safeSchemaComponents, Nil).map(enumToSealedTraitDef)

    val modelClassNames = //TODO should be created based on model and enums?
      safeSchemaComponents.map { case (key, _) =>
        SchemaRef.fromKey(key) -> key
      }
    val model = safeSchemaComponents.map {
      case (key, schema: SafeObjectSchema) =>
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
      schemas: Map[String, SafeSchema],
      path: List[String]
  ): List[Enum] =
    schemas.flatMap { case (k, v) =>
      collectEnums2(path :+ k, v)
    }.toList

  private def collectEnums2(
      path: List[String],
      schema: SafeSchema
  ): List[Enum] =
    schema match {
      case os: SafeObjectSchema =>
        os.properties.toList.flatMap { case (k, v) =>
          collectEnums2(path :+ k, v)
        }
      case schema =>
        schema.enum match {
          case list if list.nonEmpty =>
            List(Enum(path, list))
          case Nil => Nil
        }
    }

  private def collectOperations(
      openApi: SafeOpenApi,
      model: Map[SchemaRef, Defn.Class],
      modelClassNames: Map[SchemaRef, String]
  ) =
    openApi.paths.toList.flatMap { case (path, item) =>
      item.operations.map { case (method, operation) =>
        val uri = constructUrl(
          path,
          operation.parameters
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

  private def constructUrl(path: String, params: List[SafeParameter]) = {
    val pathList =
      path
        .split("\\{[^/]*\\}")
        .toList
        .dropWhile(_ == "/")
    val queryParams = params.collect { case q: SafeQueryParameter =>
      Term.Name(q.name)
    }
    val querySegments = queryParams
      .foldLeft(List.empty[String]) { (acc, item) =>
        acc match {
          case list if list.nonEmpty => list :+ s"&$item="
          case immutable.Nil         => List(s"?$item=")
        }
      }

    val pathParams = params.collect { case p: SafePathParameter =>
      Term.Name(p.name)
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
      List(Term.Name("baseUrl")) ++ pathParams ++ queryParams
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
      operation: SafeOperation,
      model: Map[SchemaRef, Defn.Class],
      basicRequestWithMethod: Term,
      schemaRefToClassName: Map[SchemaRef, String]
  ): Defn.Def = {
    val operationId = operation.operationId

    val responseClassType = operation.responses.collectFirst {
      case ("200", response) =>
        response.content
          .collectFirst { case ("application/json", jsonResponse) =>
            jsonResponse.schema match {
              case rs: SafeRefSchema =>
                Type.Name(
                  model(
                    rs.ref
                  ).name.value //TODO use schemaToClassName?
                )
            }
          }
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
      operation: SafeOperation,
      schemaRefToClassName: Map[SchemaRef, String]
  ) =
    operation.parameters
      .collect { case pathParam: SafePathParameter =>
        val paramName = Term.Name(pathParam.name)
        val paramType = optionApplication(
          schemaToType(
            "outerPathName",
            pathParam.name,
            pathParam.schema,
            schemaRefToClassName
          ),
          pathParam.required
        )
        param"$paramName : $paramType"
      }

  private def queryParameter(
      operation: SafeOperation,
      schemaRefToClassName: Map[SchemaRef, String]
  ) =
    operation.parameters
      .collect { case queryParam: SafeQueryParameter =>
        val paramName = Term.Name(queryParam.name)
        val paramType = optionApplication(
          schemaToType(
            "outerName",
            queryParam.name,
            queryParam.schema,
            schemaRefToClassName
          ),
          queryParam.required
        )
        param"$paramName : $paramType"
      }

  private def requestBodyParameter(
      operation: SafeOperation,
      model: Map[SchemaRef, Defn.Class]
  ) =
    operation.requestBody
      .flatMap { requestBody =>
        requestBody.content
          .collectFirst {
            case ("application/json", jsonRequest) =>
              jsonRequest.schema match {
                case rs: SafeRefSchema => model(rs.ref).name.value
              }
            case ("application/octet-stream", _) =>
              "File"
          }
          .map { requestClassName =>
            val paramName = Term.Name(s"a$requestClassName")
            val paramType = optionApplication(
              Type.Name(requestClassName),
              requestBody.required
            )
            param"$paramName : $paramType"
          }
      }

  private def loadOpenApi(yaml: String): SafeOpenApi = {
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
      case Some(spec) => new SafeOpenApi(spec)
      case None =>
        throw new RuntimeException(s"Failed to parse k8s swagger specs")
    }
  }

  private def enumToSealedTraitDef(enum: Enum) = {
    val name = enum.path.takeRight(2).map(_.capitalize).mkString
    val objs =
      enum.values.map(n =>
        Defn.Object(
          List(Mod.Case()),
          Term.Name(n.toString.capitalize),
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
      schema: SafeObjectSchema,
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
          schema.properties.map { case (k, v) =>
            processParams(
              name,
              k,
              v,
              schemaRefToClassName,
              schema.requiredFields.contains(k)
            )
          }.toList
        )
      ),
      Template(Nil, Nil, Self(Name(""), None), Nil)
    )

  private def processParams(
      className: String,
      name: String,
      schema: SafeSchema,
      schemaRefToClassName: Map[SchemaRef, String],
      isRequired: Boolean
  ): Term.Param = {
    val declType = schemaToType(className, name, schema, schemaRefToClassName)
    paramDeclFromType(name, optionApplication(declType, isRequired))
  }

  private def schemaToType(
      className: String,
      propertyName: String,
      schema: SafeSchema,
      schemaRefToClassName: Map[SchemaRef, String]
  ): Type =
    schema match {
      case ss: SafeStringSchema =>
        if (ss.isEnum) {
          Type.Name(
            s"${className.capitalize}${propertyName.capitalize}"
          )
        } else {
          Type.Name("String")
        }
      case si: SafeIntegerSchema =>
        if (si.isEnum) {
          Type.Name(
            s"${className.capitalize}${propertyName.capitalize}"
          )
        } else {
          Type.Name("Int")
        }
      case s: SafeArraySchema =>
        t"List[${schemaToType(className, propertyName, s.items, schemaRefToClassName)}]"
      case ref: SafeRefSchema => Type.Name(schemaRefToClassName(ref.ref))
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

case class Enum(path: List[String], values: List[Any])
case class EnumDef(st: Defn.Trait, companion: Defn.Object)
