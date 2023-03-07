package io.github.ghostbuster91.sttp.client3.openapi

import cats.implicits.catsSyntaxEq
import io.swagger.v3.oas.models.media.ComposedSchema
import sttp.model.StatusCode

import scala.collection.JavaConverters._

object OpenApiCoproductGenerator {
  def generate(openApi: SafeOpenApi): SafeOpenApi = {
    val coproducts = collectCoproducts(openApi)
    val childToParent = coproducts.values
      .flatMap(parent => parent.oneOf.map(child => child.ref -> parent))
      .toMap
    val errorsWithoutCommonParent =
      collectCandidates(openApi, childToParent, collectErrorResponses)
    val successesWithoutCommonParent =
      collectCandidates(openApi, childToParent, collectSuccessResponses)

    errorsWithoutCommonParent.foreach { case (operationId, schemas) =>
      require(
        schemas.distinct.map(_.ref.ref) === schemas
          .map(_.ref.ref)
          .distinct,
        s"$operationId cannot have different schemas for the same ref: ${schemas.distinct
            .map(_.ref)}"
      )
    }

    val newCoproducts = errorsWithoutCommonParent
      .map(kv =>
        createCoproduct(kv._1, kv._2.distinct, "GenericError")
      ) ++ successesWithoutCommonParent
      .map(kv => createCoproduct(kv._1, kv._2, "GenericSuccess"))

    registerNewSchemas(openApi, newCoproducts)
  }

  private def collectCandidates(
      openApi: SafeOpenApi,
      childToParent: Map[SchemaRef, SafeComposedSchema],
      collector: SafePathItem => List[(OperationId, SafeRefSchema)]
  ) = {
    val responses = openApi.paths.values.flatMap(collector)
    val responsePerOperation = responses
      .groupBy(_._1)
      .mapValues(t => t.map(_._2).toList)
      .toMap
    val responseWithoutCommonParent = responsePerOperation
      .filter(_._2.size >= 2)
      .filterNot { case (_, errors) =>
        errors.flatMap(e => childToParent.get(e.ref)).toSet.size == 1
      }
    responseWithoutCommonParent
  }

  private def createCoproduct(
      operationId: OperationId,
      errors: List[SafeSchema],
      postfix: String
  ) = {
    val unsafeNewSchema = new ComposedSchema
    unsafeNewSchema.setOneOf(errors.map(_.unsafe).asJava)
    s"${operationId.v.capitalize}$postfix" -> unsafeNewSchema
  }

  private def collectCoproducts(openApi: SafeOpenApi) =
    openApi.components
      .map(_.schemas)
      .getOrElse(Map.empty)
      .collect { case (k, v: SafeComposedSchema) =>
        k -> v
      }

  private def collectSuccessResponses(path: SafePathItem) =
    collectResponses(path, _.isSuccess)

  private def collectErrorResponses(path: SafePathItem) =
    collectResponses(path, _.isClientError)

  private def collectResponses(
      path: SafePathItem,
      statusCodePredicate: StatusCode => Boolean
  ) =
    path.operations.values
      .flatMap(op =>
        op.collectResponses(statusCodePredicate)
          .values
          .collect { case sr: SafeRefSchema => op.operationId -> sr }
      )
      .toList

  private def registerNewSchemas(
      openApi: SafeOpenApi,
      schemas: Map[String, ComposedSchema]
  ): SafeOpenApi = {
    openApi.components.foreach { cmp =>
      schemas.foreach { case (key, schema) =>
        cmp.unsafe.addSchemas(key, schema)
      }
    }
    openApi
  }
}
