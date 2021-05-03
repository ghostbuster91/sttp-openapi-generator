package io.github.ghostbuster91.sttp.client3

import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._

import scala.annotation.tailrec
import scala.meta._

class ProductCollector(model: Model, jsonTypeProvider: JsonTypeProvider) {
  def collect(schema: Map[SchemaRef, SafeSchema]): IM[List[Product]] = {
    val parents = model.childToParentRef.values.flatten.toSet
    schema
      .collect {
        case (key, schema: SchemaWithProperties) if !parents.contains(key) =>
          handleSchemaWithProperties(key, schema)
        case (k, schema: SafeComposedSchema) if schema.allOf.nonEmpty =>
          handleComposedSchema(k, schema)
      }
      .toList
      .sequence
  }

  private def handleComposedSchema(
      key: SchemaRef,
      schema: SafeComposedSchema
  ): IM[Product] =
    for {
      props <- schema.allOf
        .traverse(extractProperties)
        .map(_.flatten)
      additionalProps <- schema.allOf
        .traverse(extractAdditionalProperties)
        .map(_.head)
    } yield additionalProps match {
      case Some(value) =>
        Product.Open(
          model.classNameFor(key),
          schema.allOf.collect { case ref: SafeRefSchema =>
            model.classNameFor(ref.ref)
          },
          props,
          value
        )
      case None =>
        Product.Regular(
          model.classNameFor(key),
          schema.allOf.collect { case ref: SafeRefSchema =>
            model.classNameFor(ref.ref)
          },
          props
        )
    }

  private def handleSchemaWithProperties(
      key: SchemaRef,
      schema: SchemaWithProperties
  ): IM[Product] = {
    val parentClassName = model.childToParentRef
      .getOrElse(key, List.empty)
      .map(model.classNameFor)
    for {
      props <- handleRegularProp(schema)
      additionalProps <- extractAdditionalProperties(schema)
    } yield additionalProps match {
      case Some(value) =>
        Product.Open(model.classNameFor(key), parentClassName, props, value)
      case None =>
        Product.Regular(model.classNameFor(key), parentClassName, props)
    }
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

  private def handleRegularProp(schema: SchemaWithProperties) =
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
