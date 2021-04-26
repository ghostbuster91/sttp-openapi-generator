package io.github.ghostbuster91.sttp.client3.api

import io.github.ghostbuster91.sttp.client3.openapi.{
  SafeParameter,
  SafePathParameter,
  SafeQueryParameter
}
import scala.meta._

object UrlGenerator {
  def apply(path: String, params: List[SafeParameter]): Term = {
    val queryParams = params.collect { case q: SafeQueryParameter =>
      Term.Name(q.name)
    }
    val queryList = queryParamsToElements(queryParams)
    val pathList = pathFragmentsToElements(path)
    val pathAndQuery = combinePathAndQueryElements(pathList, queryList)
    val pathParams = params.collect { case p: SafePathParameter =>
      Term.Name(p.name)
    }
    Term.Interpolate(
      Term.Name("uri"),
      List(Lit.String("")) ++ pathAndQuery.map(Lit.String(_)),
      List(Term.Name("baseUrl")) ++ pathParams ++ queryParams
    )
  }

  private def pathFragmentsToElements(path: String) =
    path
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

  private def combinePathAndQueryElements(
      pathList: List[PathElement],
      queryList: List[PathElement]
  ) =
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

  private def queryParamsToElements(queryParams: List[Term.Name]) =
    queryParams
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
}
