package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._
import scala.collection.immutable.ListMap
import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import sttp.model._
import scala.meta._
import cats.data.NonEmptyList

class ApiCallGenerator(
    model: Model,
    ir: ImportRegistry,
    config: CodegenConfig
) {

  def generate(
      operations: List[CollectedOperation]
  ): Map[Option[String], List[Defn.Def]] = {
    val taggedOperations: List[(Option[String], CollectedOperation)] =
      operations.flatMap { op =>
        op.operation.tags match {
          case Some(tags) =>
            tags.map(t => Some(t) -> op)
          case None => List(None -> op)
        }
      }
    val apiCallByTag =
      taggedOperations
        .map { case (tag, op) => tag -> createApiCall(op) }
        .groupBy(_._1)
        .mapValues(_.map(_._2))

    ListMap(apiCallByTag.toSeq.sortBy(_._1): _*)
  }

  private def createApiCall(
      co: CollectedOperation
  ): Defn.Def = {
    val CollectedOperation(path, method, operation) = co
    val uri = constructUrl(
      path,
      operation.parameters
    )
    val request = createRequestCall(method, uri)
    createApiCall(operation, request)
  }

  private def createApiCall(
      operation: SafeOperation,
      basicRequestWithMethod: Term
  ): Defn.Def = {

    val functionName = Term.Name(operation.operationId)
    val fQueries = queryAsFuncParam(operation)
    val fPaths = pathAsFuncParam(operation)
    val fReqBody = reqBodyAsFuncParam(operation)
    val headerParameters = operation.parameters.collect {
      case p: SafeHeaderParameter => p
    }
    val fHeaders = headerParameters.map(headerAsFuncParam)
    val parameters =
      fPaths ++ fQueries ++ fHeaders ++ fReqBody.map(_.paramDecl)
    val modifiedReq = applyOnRequest(
      basicRequestWithMethod,
      List(
        applyHeadersToRequest(headerParameters) _
      ) ++ fReqBody.map(_.bodyApplication).toList
    )

    val response = responseSpec(operation)
    q"def $functionName(..$parameters): Request[${response.returnType}, Any] = $modifiedReq.response(${response.responseAs})"
  }

  private def responseSpec(operation: SafeOperation): ResponseSpec = {
    val successResponseSpecs =
      operation.collectResponses(statusCode => statusCode.isSuccess)
    val errorResponseSpecs =
      operation.collectResponses(statusCode => statusCode.isClientError)

    val successAncestorType =
      getCommonAncestor(successResponseSpecs.values.toList)
        .getOrElse(t"Unit")
    val errorAncestorType = getCommonAncestor(errorResponseSpecs.values.toList)

    val successCodesWithTypes = successResponseSpecs.mapValues { schema =>
      model.schemaToType(schema, isRequired = true, ir).tpe
    }

    val errorCodesWithTypes = errorResponseSpecs.mapValues { schema =>
      model.schemaToType(schema, isRequired = true, ir).tpe
    }

    val asJsonWrapper =
      (errorType: Option[Type], successType: Type) =>
        errorType match {
          case Some(value) => q"asJsonEither[$value, $successType]"
          case None        => q"asJson[$successType]"
        }

    val flattenErrors = (term: Term) =>
      if (config.handleErrors) {
        term
      } else {
        q"$term.getRight"
      }

    val responseAsCases = (successCodesWithTypes.mapValues(tpe =>
      flattenErrors(asJsonWrapper(errorAncestorType, tpe))
    ) ++ errorCodesWithTypes.mapValues(tpe =>
      flattenErrors(asJsonWrapper(Some(tpe), successAncestorType))
    )).map { case (statusCode, asJson) =>
      q"ConditionalResponseAs(_.code == StatusCode.unsafeApply($statusCode), $asJson)"
    }.toList

    val returnTpe = if (config.handleErrors) {
      errorAncestorType match {
        case Some(value) =>
          t"Either[ResponseException[$value, io.circe.Error], $successAncestorType]"
        case None =>
          t"Either[ResponseException[String, io.circe.Error], $successAncestorType]"
      }
    } else {
      successAncestorType
    }

    val topAsJson = flattenErrors(
      asJsonWrapper(errorAncestorType, successAncestorType)
    )

    ResponseSpec(
      returnTpe,
      q"""fromMetadata(
                $topAsJson,
                ..$responseAsCases
                )"""
    )
  }

  private def getCommonAncestor(errorResponseSpecs: List[SafeSchema]) =
    errorResponseSpecs match {
      case ::(head, Nil) =>
        Some(
          model
            .schemaToType(head, isRequired = true, ir)
            .tpe
        )
      case ::(head, tl) =>
        val errorAncestor = model
          .commonAncestor(
            (head :: tl).map(
              _.asInstanceOf[SafeRefSchema].ref
            ) //primitives can't be inherited
          )
          .head
        Some(model.classNameFor(errorAncestor).typeName)
      case Nil => None
    }

  private def applyOnRequest(request: Term, mods: List[Term => Term]) =
    mods
      .reduce(_ andThen _)
      .apply(request)

  private def applyHeadersToRequest(
      headers: List[SafeHeaderParameter]
  )(request: Term) =
    headers.foldLeft(request) { case (ar, h) =>
      if (h.schema.isArray || !h.required) {
        q"$ar.headers(${Term.Name(h.name)}.map(v => Header(${h.name}, v.toString())).toList:_*)"
      } else {
        q"$ar.header(${h.name}, ${Term.Name(h.name)}.toString())"
      }
    }

  private def headerAsFuncParam(headerParam: SafeHeaderParameter) = {
    val paramType = model.schemaToType(
      headerParam.schema,
      headerParam.required,
      ir
    )
    paramType.copy(paramName = headerParam.name).asParam
  }

  private def createRequestCall(method: Method, uri: Term) =
    method match {
      case Method.PUT    => q"basicRequest.put($uri)"
      case Method.GET    => q"basicRequest.get($uri)"
      case Method.POST   => q"basicRequest.post($uri)"
      case Method.DELETE => q"basicRequest.delete($uri)"
      case Method.HEAD   => q"basicRequest.head($uri)"
      case Method.PATCH  => q"basicRequest.patch($uri)"
    }

  private def constructUrl(path: String, params: List[SafeParameter]) = {
    val pathList = path
      .split('/')
      .toList
      .filter(_.nonEmpty)
      .map { s =>
        if (s.matches("\\{[^/]*\\}")) {
          PathElement.VarPath
        } else {
          PathElement.FixedPath(s)
        }
      }
    val queryParams = params.collect { case q: SafeQueryParameter =>
      Term.Name(q.name)
    }
    val queryList = queryParams
      .foldLeft(List.empty[PathElement]) { (acc, item) =>
        acc match {
          case list if list.nonEmpty =>
            list ++ List(
              PathElement.QuerySegment(s"&$item="),
              PathElement.QueryParam
            )
          case Nil =>
            List(
              PathElement.QuerySegment(s"?$item="),
              PathElement.QueryParam
            )
        }
      }

    val pathParams = params.collect { case p: SafePathParameter =>
      Term.Name(p.name)
    }
    val pathAndQuery =
      (pathList ++ queryList).foldLeft(List("")) { (acc, item) =>
        item match {
          case PathElement.FixedPath(v) =>
            val last = acc.last
            acc.dropRight(1) :+ (last ++ s"/$v")
          case PathElement.QuerySegment(q) =>
            val last = acc.last
            acc.dropRight(1) :+ (last ++ q)
          case PathElement.VarPath =>
            val last = acc.last
            acc.dropRight(1) :+ (last ++ s"/") :+ ""
          case PathElement.QueryParam => acc :+ ""
        }
      }

    Term.Interpolate(
      Term.Name("uri"),
      List(Lit.String("")) ++ pathAndQuery.map(Lit.String(_)),
      List(Term.Name("baseUrl")) ++ pathParams ++ queryParams
    )
  }

  private def pathAsFuncParam(
      operation: SafeOperation
  ): List[Term.Param] =
    operation.parameters
      .collect { case pathParam: SafePathParameter =>
        val paramType = model.schemaToType(
          pathParam.schema,
          pathParam.required,
          ir
        )
        paramType.copy(paramName = pathParam.name).asParam
      }

  private def queryAsFuncParam(
      operation: SafeOperation
  ): List[Term.Param] =
    operation.parameters
      .collect { case queryParam: SafeQueryParameter =>
        val paramType = model.schemaToType(
          queryParam.schema,
          queryParam.required,
          ir
        )
        paramType.copy(paramName = queryParam.name).asParam
      }

  private def reqBodyAsFuncParam(
      operation: SafeOperation
  ): Option[RequestBodySpec] = {
    val jsonMediaType = MediaType.ApplicationJson.toString
    val formUrlEncodedMediaType =
      MediaType.ApplicationXWwwFormUrlencoded.toString
    val octetStreamMediaType = MediaType.ApplicationOctetStream.toString
    operation.requestBody
      .flatMap { requestBody =>
        requestBody.content
          .collectFirst {
            case (`jsonMediaType`, jsonRequest) =>
              val tRef = model.schemaToType(
                jsonRequest.schema,
                requestBody.required,
                ir
              )
              RequestBodySpec(
                tRef.asParam,
                req => q"$req.body(${Term.Name(tRef.paramName)})"
              )
            case (`formUrlEncodedMediaType`, formReq) =>
              formUrlEncodedRequestBody(requestBody, formReq)
            case (`octetStreamMediaType`, _) =>
              ir.registerImport(q"import _root_.java.io.File")
              val tRef =
                if (requestBody.required) TypeRef("File", None)
                else TypeRef("File", None).asOption
              RequestBodySpec(
                tRef.asParam,
                req => q"$req.body(${Term.Name(tRef.paramName)})"
              )
          }
      }
  }
  private def formUrlEncodedRequestBody(
      requestBody: SafeRequestBody,
      formReq: SafeMediaType
  ) = {
    val tRef = model.schemaToType(
      formReq.schema,
      requestBody.required,
      ir
    )
    val schemaRef = formReq.schema match {
      case so: SafeRefSchema => so.ref
    }
    val schema = model.schemaFor(schemaRef) match {
      case so: SafeObjectSchema => so
      case other =>
        throw new IllegalArgumentException(
          s"form url encoded parameter should be object but was: $other"
        )
    }

    val topParamName = Term.Name(tRef.paramName)
    val extractors =
      schema.properties.map { case (p, pSchema) =>
        val isRequired = schema.requiredFields.contains(p)
        val stringify: Term => Term = value =>
          pSchema match {
            case _: SafeStringSchema => value
            case _                   => q"$value.toString"
          }
        val paramName = Term.Name(p)
        if (isRequired) {
          q"List($p -> $topParamName.$paramName)"
        } else {
          val paramParamName = param"$paramName"
          q"$topParamName.$paramName.map($paramParamName=> $p -> ${stringify(paramName)})"
        }
      }.toList
    RequestBodySpec(
      tRef.asParam,
      req => q"$req.body(List(..$extractors).flatten)"
    )
  }
}

private sealed trait PathElement
private object PathElement {
  case class FixedPath(v: String) extends PathElement
  case object VarPath extends PathElement
  case class QuerySegment(v: String) extends PathElement
  case object QueryParam extends PathElement
}

private case class RequestBodySpec(
    paramDecl: Term.Param,
    bodyApplication: Term => Term
)
private case class ResponseSpec(
    returnType: Type,
    responseAs: Term
)
