package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._

class OpenProductCollector(model: ModelGenerator) {
  def collect(schema: Map[String, SafeSchema]): List[OpenProduct] =
    schema.collect { case (k, schema: SafeMapSchema) =>
      OpenProduct(
        ClassName(k),
        schema.properties.map { case (k, v) =>
          PropertyName(k) -> model
            .schemaToType(
              v,
              schema.requiredFields.contains(k)
            )
            .tpe
        }
      )
    }.toList
}
