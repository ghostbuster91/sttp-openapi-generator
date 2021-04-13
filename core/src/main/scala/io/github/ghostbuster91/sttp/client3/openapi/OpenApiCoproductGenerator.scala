package io.github.ghostbuster91.sttp.client3.openapi

import io.github.ghostbuster91.sttp.client3.http.MediaType
import io.swagger.v3.oas.models.media.ComposedSchema
import scala.collection.JavaConverters._

object OpenApiCoproductGenerator {
  def generate(openApi: SafeOpenApi): SafeOpenApi = {
    val coproducts = openApi.components
      .map(_.schemas)
      .getOrElse(Map.empty)
      .collect { case (k, v: SafeComposedSchema) =>
        k -> v
      }
    val childToParent = coproducts.values
      .flatMap(parent => parent.oneOf.map(child => child.ref -> parent))
      .toMap
    val errorResponses = openApi.paths.values.flatMap { path =>
      path.operations.values.flatMap { operation =>
        operation.responses.toList.collect {
          case (statusCode, response) if statusCode.toInt >= 400 =>
            operation.operationId -> response
              .content(MediaType.ApplicationJson.v)
              .schema
              .asInstanceOf[SafeRefSchema]
        }
      }
    }
    val errorsPerOperation =
      errorResponses.groupBy(_._1).mapValues(_.map(_._2).toList)
    val errorsWithoutCommonParent = errorsPerOperation
      .filter(_._2.size >= 2)
      .filterNot { case (_, errors) =>
        errors.flatMap(e => childToParent.get(e.ref)).toSet.size == 1
      }
    val newCoproducts = errorsWithoutCommonParent.map {
      case (operationId, errors) =>
        val unsafeNewSchema = new ComposedSchema
        unsafeNewSchema.setOneOf(errors.map(_.unsafe).asJava)
        s"${operationId.capitalize}GenericError" -> unsafeNewSchema
    }
    openApi.components.foreach { cmp =>
      newCoproducts.foreach { case (key, schema) =>
        cmp.unsafe.addSchemas(key, schema)
      }
    }
    openApi
  }
}
