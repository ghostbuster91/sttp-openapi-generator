package io.github.ghostbuster91.sttp.client3.openapi

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.media.Schema
import io.github.ghostbuster91.sttp.client3.http.MediaType
import io.github.ghostbuster91.sttp.client3.openapi._

object OpenApiEnumFlattener {
  def flatten(openApi: OpenAPI): SafeOpenApi = {
    val safeApi = new SafeOpenApi(openApi)
    val schemas: List[(String, SchemaWithReassign)] = safeApi.components
      .map { cmp =>
        cmp.schemas.toList.map { case (k, v) =>
          k -> SchemaWithReassign(
            v,
            _ => () //intentionally noop as this is the place where it should be
          )
        }
      }
      .getOrElse(List.empty)
    val requestBodies =
      safeApi.components.map(_.requestBodies).getOrElse(Map.empty)
    val rbSchemas: List[(String, SchemaWithReassign)] =
      requestBodies.toList.flatMap { case (k, rb) =>
        rb.content
          .get(MediaType.ApplicationJson.v)
          .map(mt =>
            k -> SchemaWithReassign(
              mt.schema,
              s => mt.unsafe.setSchema(s.unsafe)
            )
          )
      }
    val operationParameters: List[(String, SchemaWithReassign)] =
      safeApi.paths.values
        .flatMap(_.operations.values)
        .flatMap(_.parameters)
        .map(p =>
          p.name -> SchemaWithReassign(
            p.schema,
            s => p.unsafe.setSchema(s.unsafe)
          )
        )
        .toList
    val operationReqBodies: List[(String, SchemaWithReassign)] =
      safeApi.paths.values
        .flatMap(_.operations.values)
        .flatMap(o =>
          o.requestBody.flatMap(
            _.content
              .get(MediaType.ApplicationJson.v)
              .map(mt =>
                o.operationId -> SchemaWithReassign(
                  mt.schema,
                  s => mt.unsafe.setSchema(s.unsafe)
                )
              )
          )
        )
        .toList
    val operationResponses: List[(String, SchemaWithReassign)] =
      safeApi.paths.values
        .flatMap(_.operations.values)
        .flatMap(o =>
          o.responses.values
            .flatMap(_.content.get(MediaType.ApplicationJson.v))
            .map(mt =>
              o.operationId -> SchemaWithReassign(
                mt.schema,
                s => mt.unsafe.setSchema(s.unsafe)
              )
            )
        )
        .toList
    val enums = collectEnums(
      schemas ++ rbSchemas ++ operationParameters ++ operationReqBodies ++ operationResponses
    ) //todo there can naming conflicts with other entities
    val nameMap = generateUniqueName(NameGeneratorProgress(), enums).nameMap
    nameMap.foreach { case (name, enum) =>
      safeApi.components.get.unsafe.addSchemas(name, enum.swr.schema.unsafe)
      val unsafeNewSchema = new Schema[Object]
      unsafeNewSchema.set$ref(SchemaRef.schema(name).ref)
      enum.swr.reassign(new SafeRefSchema(unsafeNewSchema))
    }
    safeApi
  }

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
  case class NameGeneratorProgress(
      nameMap: Map[String, Enum] = Map.empty,
      nameCounter: Map[String, Int] = Map.empty
  )
  case class Enum(propertyName: String, swr: SchemaWithReassign)
  case class SchemaWithReassign(
      schema: SafeSchema,
      reassign: SafeSchema => Unit
  )
}
