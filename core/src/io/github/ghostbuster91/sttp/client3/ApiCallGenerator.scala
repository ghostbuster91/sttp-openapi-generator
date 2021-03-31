package io.github.ghostbuster91.sttp.client3

import scala.meta._

class ApiCallGenerator(modelGenerator: ModelGenerator) {

  def generate(
      operations: List[CollectedOperation],
  ): Map[Option[String], List[Defn.Def]] = {
    val taggedOperations: List[(Option[String], CollectedOperation)] =
      operations.flatMap { op =>
        op.operation.tags match {
          case Some(tags) =>
            tags.map(t => Some(t) -> op)
          case None => List(None -> op)
        }
      }
    val apiCallByTag = taggedOperations
      .map { case (tag, op) => tag -> createApiCall(op) }
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .toMap
    apiCallByTag
  }

  private def createApiCall(
      co: CollectedOperation,
  ): Defn.Def = {
    val CollectedOperation(path, method, operation) = co
    val uri = constructUrl(
      path,
      operation.parameters,
    )
    val request = createRequestCall(method, uri)
    createApiCall(
      operation,
      request,
    )
  }

  private def createApiCall(
      operation: SafeOperation,
      basicRequestWithMethod: Term,
  ): Defn.Def = {
    val operationId = operation.operationId

    val responseClassType = operation.responses
      .collectFirst { case ("200", response) =>
        response.content
          .collectFirst { case ("application/json", jsonResponse) =>
            jsonResponse.schema match {
              case rs: SafeRefSchema =>
                Type.Name(modelGenerator.classNameFor(rs.ref))
            }
          }

      }
      .flatten
      .getOrElse(Type.Name("Unit"))

    val functionName = Term.Name(operationId)
    val queryParameters = queryParameter(operation)
    val pathParameters = pathParameter(operation)
    val bodyParameter = requestBodyParameter(operation)
    val parameters = pathParameters ++ queryParameters ++ bodyParameter
    val body: Term = Term.Apply(
      Term.Select(
        bodyParameter
          .map(p =>
            Term.Apply(
              Term.Select(basicRequestWithMethod, Term.Name("body")),
              List(Term.Name(p.name.value)),
            ),
          )
          .getOrElse(basicRequestWithMethod),
        Term.Name("response"),
      ),
      List(q"asJson[$responseClassType].getRight"),
    )
    q"def $functionName(..$parameters): Request[$responseClassType, Any] = $body"
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
          case Nil                   => List(s"?$item=")
        }
      }

    val pathParams = params.collect { case p: SafePathParameter =>
      Term.Name(p.name)
    }
    val pathAndQuery = (pathList.dropRight(1) ++ List(
      pathList.lastOption.getOrElse("") ++ querySegments.headOption.getOrElse(
        "",
      ),
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
      List(Term.Name("baseUrl")) ++ pathParams ++ queryParams,
    )
  }

  private def pathParameter(
      operation: SafeOperation,
  ) =
    operation.parameters
      .collect { case pathParam: SafePathParameter =>
        val paramName = Term.Name(pathParam.name)
        val paramType = modelGenerator.schemaToType(
          "outerPathName",
          pathParam.name,
          pathParam.schema,
          pathParam.required,
        )
        param"$paramName : $paramType"
      }

  private def queryParameter(
      operation: SafeOperation,
  ) =
    operation.parameters
      .collect { case queryParam: SafeQueryParameter =>
        val paramName = Term.Name(queryParam.name)
        val paramType = modelGenerator.schemaToType(
          "outerName",
          queryParam.name,
          queryParam.schema,
          queryParam.required,
        )
        param"$paramName : $paramType"
      }

  private def requestBodyParameter(
      operation: SafeOperation,
  ) =
    operation.requestBody
      .flatMap { requestBody =>
        requestBody.content
          .collectFirst {
            case ("application/json", jsonRequest) =>
              jsonRequest.schema match {
                case rs: SafeRefSchema => modelGenerator.classNameFor(rs.ref)
              }
            case ("application/octet-stream", _) =>
              "File"
          }
          .map { requestClassName =>
            val paramName = Term.Name(s"a$requestClassName")
            val paramType = ModelGenerator.optionApplication(
              Type.Name(requestClassName),
              requestBody.required,
            )
            param"$paramName : $paramType"
          }
      }
}
