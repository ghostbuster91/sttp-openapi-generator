package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.json._
import io.github.ghostbuster91.sttp.client3.openapi._
import io.github.ghostbuster91.sttp.client3.ModelGenerator._
import _root_.io.github.ghostbuster91.sttp.client3.model.{Discriminator => _, _}
import cats.Eval
import cats.data.IndexedStateT

import scala.annotation.tailrec
import scala.meta._

class ModelGenerator(
    model: Model,
    jsonTypeProvider: JsonTypeProvider
) {
  def generate(coproducts: List[Coproduct]): IM[List[Defn]] =
    for {
      classes <- collectClasses(model.schemas)
    } yield collectTraits(coproducts) ++ classes

  private def collectClasses(schemas: Map[SchemaRef, SafeSchema]) = {
    val allOfParents = schemas
      .collect { case (_, schema: SafeComposedSchema) =>
        schema.allOf.collect { case ref: SafeRefSchema => ref.ref }
      }
      .flatten
      .toSet
    schemas
      .filterKeys(!allOfParents.contains(_))
      .collect {
        case (key, schema: SchemaWithProperties) =>
          val parentClassName = model.childToParentRef
            .getOrElse(key, List.empty)
            .map(model.classNameFor)

          Product(
            model.classNameFor(key),
            schema.properties.map { case (k, v) =>
              Property(k, v, schema.requiredFields.contains(k))
            }.toList,
            parentClassName,
            schema match {
              case mp: SafeMapSchema =>
                mp.additionalProperties
              case _ => Left(false)
            }
          )
        case (key, schema: SafeComposedSchema) if schema.allOf.nonEmpty =>
          Product(
            model.classNameFor(key),
            schema.allOf.flatMap(extractProperties),
            schema.allOf.collect { case ref: SafeRefSchema =>
              model.classNameFor(ref.ref)
            },
            additionalProperties = schema.allOf
              .collectFirst { case ref: SafeRefSchema =>
                model.schemaFor(ref.ref) match {
                  case s: SafeMapSchema => s.additionalProperties
                  case _                => Left(false)
                }
              }
              .getOrElse(Left(false))
          )
      }
      .toList
      .traverse(schemaToClassDef)
  }

  @tailrec
  private def extractAdditionalProperties(
      schema: SafeSchema
  ): Either[Boolean, SafeSchema] =
    schema match {
      case s: SafeRefSchema =>
        extractAdditionalProperties(model.schemaFor(s.ref))
      case s: SafeMapSchema => s.additionalProperties
      case _                => Left(false)
    }

  @tailrec
  private def extractProperties(schema: SafeSchema): List[Property] =
    schema match {
      case s: SafeRefSchema        => extractProperties(model.schemaFor(s.ref))
      case s: SchemaWithProperties => extractObjectProperties(s)
    }

  private def extractObjectProperties(
      schema: SchemaWithProperties
  ): List[Property] =
    schema.properties.map { case (k, v) =>
      Property(k, v, schema.requiredFields.contains(k))
    }.toList

  private def collectTraits(coproducts: List[Coproduct]) =
    coproducts.map(schemaToSealedTrait)

  private def handleAdditionalProps(
      schema: Either[Boolean, SafeSchema]
  ): IM[Option[ParameterRef]] =
    schema match {
      case Right(schema) =>
        model.schemaToType(schema).map { tpe =>
          Some(
            ParameterRef(
              t"Map[String, $tpe]",
              ParameterName("_additionalProperties"),
              None
            )
          )
        }
      case Left(true) =>
        jsonTypeProvider.AnyType.map(anyType =>
          Some(
            ParameterRef(
              t"Map[String, $anyType]",
              ParameterName("_additionalProperties"),
              None
            )
          )
        )
      case Left(false) => Option.empty[ParameterRef].pure[IM]
    }

  private def schemaToClassDef(
      product: Product
  ): IM[Defn] =
    for {
      props <- product.properties
        .traverse(processParams)
      additionalProperties <- handleAdditionalProps(
        product.additionalProperties
      )
      adjustedProps = (props ++ additionalProperties).map(_.asParam)
    } yield product.parents match {
      case parents if parents.nonEmpty =>
        val parentInits = parents.sortBy(_.v).map(p => init"${p.typeName}()")
        q"case class ${product.name.typeName}(..$adjustedProps) extends ..$parentInits"
      case Nil =>
        q"case class ${product.name.typeName}(..$adjustedProps)"
    }

  private def schemaToSealedTrait(
      coproduct: Coproduct
  ): Defn.Trait = {
    val defParams =
      (coproduct.properties ++ coproduct.additionalProperties).map(_.asDef)

    q"""sealed trait ${coproduct.name.typeName} {
            ..$defParams
        }
        """
  }
  private def processParams(
      property: Property
  ): IM[ParameterRef] =
    model
      .schemaToParameter(
        property.schema,
        property.isRequired
      )
      .map(_.withName(property.name))
}

object ModelGenerator {
  def apply(
      model: Model,
      jsonTypeProvider: JsonTypeProvider
  ): ModelGenerator =
    new ModelGenerator(
      model,
      jsonTypeProvider
    )

  private case class Product(
      name: ClassName,
      properties: List[Property],
      parents: List[ClassName],
      additionalProperties: Either[Boolean, SafeSchema]
  )
  private case class Property(
      name: String,
      schema: SafeSchema,
      isRequired: Boolean
  )
}
