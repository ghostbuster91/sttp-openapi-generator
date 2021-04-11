package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.http.Method
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.http.MediaType
import scala.collection.immutable.ListMap
import scala.meta._

class ApiCallGenerator(modelGenerator: ModelGenerator, ir: ImportRegistry) {

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
        .toMap

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
    val operationId = operation.operationId

    val responseClassType = operation.responses
      .collectFirst { case ("200", response) =>
        response.content
          .collectFirst { case (MediaType.ApplicationJson.v, jsonResponse) =>
            modelGenerator
              .schemaToType(
                jsonResponse.schema,
                isRequired = true
              )
              .tpe
          }

      }
      .flatten
      .getOrElse(Type.Name("Unit"))

    val functionName = Term.Name(operationId)
    val fQueries = queryAsFuncParam(operation)
    val fPaths = pathAsFuncParam(operation)
    val fReqBody = reqBodyAsFuncParam(operation)
    val headerParameters = operation.parameters.collect {
      case p: SafeHeaderParameter => p
    }
    val fHeaders = headerParameters.map(headerAsFuncParam)
    val parameters =
      fPaths ++ fQueries ++ fHeaders ++ fReqBody.map(_.paramDecl)
    val body: Term = Term.Apply(
      Term.Select(
        (List(
          applyHeadersToRequest(headerParameters) _
        ) ++ fReqBody.map(_.bodyApplication).toList)
          .reduce(_ andThen _)
          .apply(basicRequestWithMethod),
        Term.Name("response")
      ),
      List(q"asJson[$responseClassType].getRight")
    )
    q"def $functionName(..$parameters): Request[$responseClassType, Any] = $body"
  }

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
    val paramType = modelGenerator.schemaToType(
      headerParam.schema,
      headerParam.required
    )
    paramType.copy(paramName = headerParam.name).asParam
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
        val paramType = modelGenerator.schemaToType(
          pathParam.schema,
          pathParam.required
        )
        paramType.copy(paramName = pathParam.name).asParam
      }

  private def queryAsFuncParam(
      operation: SafeOperation
  ): List[Term.Param] =
    operation.parameters
      .collect { case queryParam: SafeQueryParameter =>
        val paramType = modelGenerator.schemaToType(
          queryParam.schema,
          queryParam.required
        )
        paramType.copy(paramName = queryParam.name).asParam
      }

  private def reqBodyAsFuncParam(
      operation: SafeOperation
  ): Option[BodySpec] =
    operation.requestBody
      .flatMap { requestBody =>
        requestBody.content
          .collectFirst {
            case (MediaType.ApplicationJson.v, jsonRequest) =>
              val tRef = modelGenerator.schemaToType(
                jsonRequest.schema,
                requestBody.required
              )
              BodySpec(
                tRef.asParam,
                req => q"$req.body(${Term.Name(tRef.paramName)})"
              )
            case (MediaType.FormUrlEncoded.v, formReq) =>
              formUrlEncodedRequestBody(requestBody, formReq)

            case (MediaType.ApplicationOctetStream.v, _) =>
              ir.registerImport(q"import _root_.java.io.File")
              val tRef = ModelGenerator.optionApplication(
                TypeRef("File", None),
                requestBody.required,
                false
              )
              BodySpec(
                tRef.asParam,
                req => q"$req.body(${Term.Name(tRef.paramName)})"
              )
          }
      }
  private def formUrlEncodedRequestBody(
      requestBody: SafeRequestBody,
      formReq: SafeMediaType
  ) = {
    val tRef = modelGenerator.schemaToType(
      formReq.schema,
      requestBody.required
    )
    val schemaRef = formReq.schema match {
      case so: SafeRefSchema => so.ref
    }
    val schema = modelGenerator.schemaFor(schemaRef) match {
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
    BodySpec(
      tRef.asParam,
      req => q"$req.body(List(..$extractors).flatten)"
    )
  }
}

sealed trait PathElement
object PathElement {
  case class FixedPath(v: String) extends PathElement
  case object VarPath extends PathElement
  case class QuerySegment(v: String) extends PathElement
  case object QueryParam extends PathElement
}

case class BodySpec(paramDecl: Term.Param, bodyApplication: Term => Term)
