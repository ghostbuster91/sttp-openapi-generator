package io.github.ghostbuster91.sttp.client3

import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._

import scala.annotation.tailrec
import scala.meta._

class OpenProductCollector(model: Model, jsonTypeProvider: JsonTypeProvider) {
  def collect(schema: Map[String, SafeSchema]): IM[List[OpenProduct]] = {
    val parents = model.childToParentRef.values.flatten.map(_.key).toSet
    schema
      .collect {
        case (k, schema: SafeMapSchema) if !parents.contains(k) =>
          for {
            props <- handleRegularProp(schema)
            additionalProps <- handleAdditionalProps(schema)
          } yield Option(OpenProduct(ClassName(k), props, additionalProps))
        case (k, schema: SafeComposedSchema) if schema.allOf.nonEmpty =>
          for {
            props <- schema.allOf
              .traverse(extractProperties)
              .map(_.flatten)
            additionalProps <- schema.allOf
              .traverse(extractAdditionalProperties)
              .map(_.head)
          } yield additionalProps.map(OpenProduct(ClassName(k), props, _))
      }
      .toList
      .sequence
      .map(_.flatten)
  }

  @tailrec
  private def extractAdditionalProperties(
      schema: SafeSchema
  ): IM[Option[ParameterRef]] =
    schema match {
      case s: SafeRefSchema =>
        extractAdditionalProperties(model.schemaFor(s.ref))
      case s: SafeMapSchema => handleAdditionalProps(s).map(Some(_))
      case _                => Option.empty[ParameterRef].pure[IM]
    }

  @tailrec
  private def extractProperties(schema: SafeSchema): IM[List[ParameterRef]] =
    schema match {
      case s: SafeRefSchema        => extractProperties(model.schemaFor(s.ref))
      case s: SchemaWithProperties => extractObjectProperties(s)
    }

  private def extractObjectProperties(
      schema: SchemaWithProperties
  ): IM[List[ParameterRef]] =
    schema.properties.toList.traverse { case (k, v) =>
      model
        .schemaToParameter(v, schema.requiredFields.contains(k))
        .map(_.withName(k))
    }

  private def handleRegularProp(schema: SafeMapSchema) =
    schema.properties
      .map { case (k, v) =>
        model
          .schemaToParameter(v, schema.requiredFields.contains(k))
          .map(_.withName(k))
      }
      .toList
      .sequence

  private def handleAdditionalProps(schema: SafeMapSchema) =
    schema.additionalProperties match {
      case Left(_) =>
        jsonTypeProvider.AnyType.map(any =>
          ParameterRef(
            t"Map[String, $any]",
            ParameterName("_additionalProperties"),
            None
          )
        )
      case Right(value) =>
        model
          .schemaToType(value)
          .map(f =
            s =>
              ParameterRef(
                t"Map[String, $s]",
                ParameterName("_additionalProperties"),
                None
              )
          )
    }
}
