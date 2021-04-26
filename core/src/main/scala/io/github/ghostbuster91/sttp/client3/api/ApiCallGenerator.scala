package io.github.ghostbuster91.sttp.client3.api

import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._
import io.github.ghostbuster91.sttp.client3._
import sttp.model._

import scala.collection.immutable.ListMap
import scala.meta._

class ApiCallGenerator(
    model: Model,
    config: CodegenConfig,
    jsonTypeProvider: JsonTypeProvider
) {

  def generate(
      operations: List[CollectedOperation]
  ): IM[Map[Option[String], List[Defn.Def]]] =
    for {
      taggedCalls <- collectOperationsByTag(operations)
        .traverse { case (tag, op) =>
          createApiCall(op).map(apiCall => tag -> apiCall)
        }
      apiCallByTag = taggedCalls
        .groupBy(_._1)
        .mapValues(_.map(_._2))
    } yield ListMap(apiCallByTag.toSeq.sortBy(_._1): _*)

  private def collectOperationsByTag(operations: List[CollectedOperation]) =
    operations.flatMap { op =>
      op.operation.tags match {
        case Some(tags) =>
          tags.map(t => Some(t) -> op)
        case None => List(None -> op)
      }
    }

  private def createApiCall(
      co: CollectedOperation
  ): IM[Defn.Def] = {
    val CollectedOperation(path, method, operation) = co
    val uri = UrlGenerator(path, operation.parameters)
    val request = createRequestCall(method, uri)
    createApiCall(operation, request)
  }

  private def createApiCall(
      operation: SafeOperation,
      basicRequestWithMethod: Term
  ): IM[Defn.Def] = {
    val functionName = Term.Name(operation.operationId)
    val headerParameters = operation.parameters.collect {
      case p: SafeHeaderParameter => p
    }
    for {
      fQueries <- queryAsFuncParam(operation)
      fPaths <- pathAsFuncParam(operation)
      fReqBody <- reqBodyAsFuncParam(operation)
      fHeaders <- headerParameters.traverse(headerAsFuncParam)
      response <- new ResponseSpecGenerator(config, model, jsonTypeProvider)(
        operation
      )
    } yield {
      val parameters =
        fPaths ++ fQueries ++ fHeaders ++ fReqBody.map(_.paramDecl)
      val modifiedReq = applyOnRequest(
        basicRequestWithMethod,
        List(
          applyHeadersToRequest(headerParameters) _
        ) ++ fReqBody.map(_.bodyApplication).toList
      )

      q"def $functionName(..$parameters): Request[${response.returnType}, Any] = $modifiedReq.response(${response.responseAs})"
    }
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

  private def headerAsFuncParam(
      headerParam: SafeHeaderParameter
  ): IM[Term.Param] =
    model
      .schemaToType(
        headerParam.schema,
        headerParam.required
      )
      .map(paramType => paramType.copy(paramName = headerParam.name).asParam)

  private def createRequestCall(method: Method, uri: Term) =
    method match {
      case Method.PUT    => q"basicRequest.put($uri)"
      case Method.GET    => q"basicRequest.get($uri)"
      case Method.POST   => q"basicRequest.post($uri)"
      case Method.DELETE => q"basicRequest.delete($uri)"
      case Method.HEAD   => q"basicRequest.head($uri)"
      case Method.PATCH  => q"basicRequest.patch($uri)"
    }

  private def pathAsFuncParam(
      operation: SafeOperation
  ): IM[List[Term.Param]] =
    operation.parameters.collect { case pathParam: SafePathParameter =>
      model
        .schemaToType(
          pathParam.schema,
          pathParam.required
        )
        .map(paramType => paramType.copy(paramName = pathParam.name).asParam)
    }.sequence

  private def queryAsFuncParam(
      operation: SafeOperation
  ): IM[List[Term.Param]] =
    operation.parameters.collect { case queryParam: SafeQueryParameter =>
      model
        .schemaToType(
          queryParam.schema,
          queryParam.required
        )
        .map { paramType =>
          paramType.copy(paramName = queryParam.name).asParam
        }
    }.sequence

  private def reqBodyAsFuncParam(
      operation: SafeOperation
  ): IM[Option[RequestBodySpec]] = {
    val jsonMediaType = MediaType.ApplicationJson.toString
    val formUrlEncodedMediaType =
      MediaType.ApplicationXWwwFormUrlencoded.toString
    val octetStreamMediaType = MediaType.ApplicationOctetStream.toString
    operation.requestBody.flatMap { requestBody =>
      requestBody.content
        .collectFirst {
          case (`jsonMediaType`, jsonRequest) =>
            handleJsonRequestBody(requestBody, jsonRequest)
          case (`formUrlEncodedMediaType`, formReq) =>
            formUrlEncodedRequestBody(requestBody, formReq)
          case (`octetStreamMediaType`, _) =>
            handleOctetStreamRequestBody(requestBody)
        }
    }.sequence
  }

  private def handleOctetStreamRequestBody(requestBody: SafeRequestBody) =
    ImportRegistry
      .registerExternalTpe(q"import _root_.java.io.File")
      .map { tFile =>
        val tRef =
          if (requestBody.required) TypeRef(tFile)
          else TypeRef(tFile).asOption
        RequestBodySpec(
          tRef.asParam,
          req => q"$req.body(${Term.Name(tRef.paramName)})"
        )
      }

  private def handleJsonRequestBody(
      requestBody: SafeRequestBody,
      jsonRequest: SafeMediaType
  ) =
    model
      .schemaToType(
        jsonRequest.schema,
        requestBody.required
      )
      .map { tRef =>
        RequestBodySpec(
          tRef.asParam,
          req => q"$req.body(${Term.Name(tRef.paramName)})"
        )
      }

  private def formUrlEncodedRequestBody(
      requestBody: SafeRequestBody,
      formReq: SafeMediaType
  ): IM[RequestBodySpec] = {
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
    model
      .schemaToType(
        formReq.schema,
        requestBody.required
      )
      .map { tRef =>
        val topParamName = Term.Name(tRef.paramName)
        val extractors = formBodyExtractors(schema, topParamName)
        RequestBodySpec(
          tRef.asParam,
          req => q"$req.body(List(..$extractors).flatten)"
        )
      }
  }

  private def formBodyExtractors(
      schema: SafeObjectSchema,
      topParamName: Term.Name
  ) =
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
