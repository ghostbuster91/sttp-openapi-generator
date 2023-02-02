package io.github.ghostbuster91.sttp.client3.openapi

import io.swagger.v3.oas.models.media.Schema
import sttp.model.MediaType

import scala.annotation.tailrec

object OpenApiEnumFlattener {
  def flatten(safeApi: SafeOpenApi): SafeOpenApi = {
    val schemas = collectComponentsSchemas(safeApi)
    val rbSchemas = collectRequestBodiesSchemas(safeApi)
    val operationSchemas = collectFromOperations(safeApi)
    val enums = collectEnums(
      schemas ++ rbSchemas ++ operationSchemas
    ) // todo there can naming conflicts with other entities
    val nameMap = generateUniqueName(NameGeneratorProgress(), enums).nameMap
    registerEnumsAndUpdateLinks(safeApi, nameMap)
  }

  private def registerEnumsAndUpdateLinks(
      safeApi: SafeOpenApi,
      nameMap: Map[String, Enum]
  ): SafeOpenApi = {
    nameMap.foreach { case (name, enum) =>
      safeApi.components.get.unsafe.addSchemas(name, enum.swr.schema.unsafe)
      val unsafeNewSchema = new Schema[Object]
      unsafeNewSchema.set$ref(SchemaRef.schema(name).ref)
      enum.swr.reassign(new SafeRefSchema(unsafeNewSchema))
    }
    safeApi
  }

  private def collectComponentsSchemas(
      safeApi: SafeOpenApi
  ): List[(String, SchemaWithReassign)] =
    safeApi.components
      .map { cmp =>
        cmp.schemas.toList.map { case (k, v) =>
          k -> SchemaWithReassign(
            v,
            _ =>
              () // intentionally noop as this is the place where it should be
          )
        }
      }
      .getOrElse(List.empty)

  private def collectRequestBodiesSchemas(
      safeApi: SafeOpenApi
  ): List[(String, SchemaWithReassign)] = {
    val requestBodies = safeApi.components
      .map(_.requestBodies)
      .getOrElse(Map.empty)
    requestBodies.toList.flatMap { case (k, rb) =>
      rb.content
        .get(MediaType.ApplicationJson.toString)
        .map(mt =>
          k -> SchemaWithReassign(
            mt.schema,
            s => mt.unsafe.setSchema(s.unsafe)
          )
        )
    }
  }

  private def collectFromOperations(
      safeApi: SafeOpenApi
  ): List[(String, SchemaWithReassign)] =
    safeApi.paths.values
      .flatMap(_.operations.values)
      .flatMap(operation =>
        collectOperationParameters(operation) ++
          collectOperationRequestBodies(operation) ++
          collectOperationResponses(operation)
      )
      .toList

  private def collectOperationParameters(
      operation: SafeOperation
  ): List[(String, SchemaWithReassign)] =
    operation.parameters
      .map(p =>
        p.name -> SchemaWithReassign(
          p.schema,
          s => p.unsafe.setSchema(s.unsafe)
        )
      )

  private def collectOperationRequestBodies(
      operation: SafeOperation
  ): List[(String, SchemaWithReassign)] =
    operation.requestBody
      .flatMap(_.content.get(MediaType.ApplicationJson.toString))
      .map(mt =>
        operation.operationId.v -> SchemaWithReassign(
          mt.schema,
          s => mt.unsafe.setSchema(s.unsafe)
        )
      )
      .toList

  private def collectOperationResponses(
      operation: SafeOperation
  ): List[(String, SchemaWithReassign)] =
    operation.responses.values
      .flatMap(_.content.get(MediaType.ApplicationJson.toString))
      .map(mt =>
        operation.operationId.v -> SchemaWithReassign(
          mt.schema,
          s => mt.unsafe.setSchema(s.unsafe)
        )
      )
      .toList

  @tailrec
  private def generateUniqueName(
      progress: NameGeneratorProgress,
      input: List[Enum]
  ): NameGeneratorProgress =
    input match {
      case head :: tail =>
        val propertyName = head.propertyName.capitalize
        val counter = progress.nameCounter.getOrElse(propertyName, 0)
        val alteredName = if (counter > 0) {
          s"$propertyName${counter + 1}"
        } else {
          propertyName
        }
        val newProgress = progress.copy(
          nameMap = progress.nameMap + (alteredName -> head),
          progress.nameCounter + (propertyName -> (counter + 1))
        )
        generateUniqueName(newProgress, tail)
      case Nil => progress
    }

  private def collectEnums(
      schemas: List[(String, SchemaWithReassign)]
  ): List[Enum] =
    schemas.flatMap { case (k, v) =>
      collectEnumsFromSingleSchema(k, v)
    }

  private def collectEnumsFromSingleSchema(
      propertyName: String,
      swr: SchemaWithReassign
  ): List[Enum] =
    swr.schema match {
      case os: SafeObjectSchema =>
        os.properties.toList.flatMap { case (k, v) =>
          collectEnumsFromSingleSchema(
            k,
            SchemaWithReassign(v, s => os.unsafe.addProperties(k, s.unsafe))
          )
        }
      case s: SafeSchema if s.isEnum =>
        List(Enum(propertyName, swr))
      case _ => Nil
    }

  private case class NameGeneratorProgress(
      nameMap: Map[String, Enum] = Map.empty,
      nameCounter: Map[String, Int] = Map.empty
  )
  private case class Enum(propertyName: String, swr: SchemaWithReassign)
  private case class SchemaWithReassign(
      schema: SafeSchema,
      reassign: SafeSchema => Unit
  )
}
