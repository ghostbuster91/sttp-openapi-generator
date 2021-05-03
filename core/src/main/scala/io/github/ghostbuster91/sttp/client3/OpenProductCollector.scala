package io.github.ghostbuster91.sttp.client3

import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._
import scala.meta._

class OpenProductCollector(model: Model, jsonTypeProvider: JsonTypeProvider) {
  def collect(schema: Map[String, SafeSchema]): IM[List[OpenProduct]] =
    schema
      .collect { case (k, schema: SafeMapSchema) =>
        for {
          props <- handleRegularProp(schema)
          additionalProps <- handleAdditionalProps(schema)
        } yield OpenProduct(ClassName(k), props, additionalProps)
      }
      .toList
      .sequence

  private def handleRegularProp(schema: SafeMapSchema) =
    schema.properties
      .map { case (k, v) =>
        model
          .schemaToType(v, schema.requiredFields.contains(k))
          .map(_.copy(paramName = PropertyName(k)))
      }
      .toList
      .sequence

  private def handleAdditionalProps(schema: SafeMapSchema) =
    schema.additionalProperties match {
      case Left(_) =>
        jsonTypeProvider.AnyType.map(any =>
          TypeRef(
            t"Map[String, $any]",
            PropertyName("_additionalProperties"),
            None
          )
        )
      case Right(value) =>
        model
          .schemaToType(value, isRequired = true)
          .map(f =
            s =>
              TypeRef(
                t"Map[String, ${s.tpe}]",
                PropertyName("_additionalProperties"),
                None
              )
          )
    }
}
