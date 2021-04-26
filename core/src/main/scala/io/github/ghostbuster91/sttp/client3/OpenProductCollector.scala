package io.github.ghostbuster91.sttp.client3

import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._

class OpenProductCollector(model: Model) {
  def collect(schema: Map[String, SafeSchema]): IM[List[OpenProduct]] =
    schema
      .collect { case (k, schema: SafeMapSchema) =>
        schema.properties
          .map { case (k, v) =>
            model
              .schemaToType(
                v,
                schema.requiredFields.contains(k)
              )
              .map(x => PropertyName(k) -> x.tpe)

          }
          .toList
          .sequence
          .map(props => OpenProduct(ClassName(k), props.toMap))
      }
      .toList
      .sequence
}
