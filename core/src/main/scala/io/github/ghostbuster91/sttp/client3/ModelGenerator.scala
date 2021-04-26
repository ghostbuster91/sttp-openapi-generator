package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.json._
import io.github.ghostbuster91.sttp.client3.openapi._
import scala.meta._

class ModelGenerator(
    model: Model,
    jsonTypeProvider: JsonTypeProvider
) {
  def generate: IM[Map[SchemaRef, Defn]] =
    for {
      classes <- collectClasses(model.schemas)
      traits <- collectTraits(model.schemas)
    } yield traits ++ classes

  private def collectClasses(schemas: Map[SchemaRef, SafeSchema]) =
    schemas
      .collect { case (key, schema: SchemaWithProperties) =>
        val parentClassName = model.childToParentRef
          .getOrElse(key, List.empty)
          .map(model.classNames.apply)
        schemaToClassDef(
          model.classNames(key),
          schema,
          parentClassName
        ).map(classDef => key -> classDef)
      }
      .toList
      .sequence
      .map(_.toMap)

  private def collectTraits(schemas: Map[SchemaRef, SafeSchema]) = {
    val parentToChilds = model.schemas.collect {
      case (key, composed: SafeComposedSchema) =>
        key -> composed.oneOf.map(_.ref)
    }
    schemas
      .collect { case (key, composed: SafeComposedSchema) =>
        val childSchemas = parentToChilds(key)
          .map(c => model.schemas(c).asInstanceOf[SafeObjectSchema])
        schemaToSealedTrait(
          model.classNames(key),
          composed.discriminator,
          childSchemas
        ).map(traitDef => key -> traitDef)
      }
      .toList
      .sequence
      .map(_.toMap)
  }

  private def schemaToClassDef(
      name: String,
      schema: SchemaWithProperties,
      parentClassName: List[String]
  ): IM[Defn] = {
    val className = Type.Name(name)
    for {
      props <- schema.properties.toList
        .traverse { case (k, v) =>
          processParams(
            k,
            v,
            schema.requiredFields.contains(k)
          )
        }
      adjustedProps <- schema match {
        case _: SafeMapSchema =>
          jsonTypeProvider.AnyType.map(anyType =>
            props :+ param"_additionalProperties: Map[String, $anyType]"
          )
        case _: SafeObjectSchema => props.pure[IM]
      }
    } yield parentClassName match {
      case parents if parents.nonEmpty =>
        val parentInits = parents.sorted.map(p => init"${Type.Name(p)}()")
        q"case class $className(..$adjustedProps) extends ..$parentInits"
      case Nil =>
        q"case class $className(..$adjustedProps)"
    }
  }

  private def schemaToSealedTrait(
      name: String,
      discriminator: Option[SafeDiscriminator],
      childs: List[SafeObjectSchema]
  ): IM[Defn.Trait] = {
    val traitName = Type.Name(name)
    discriminator match {
      case Some(d) =>
        val child = childs.head
        val discriminatorProperty = child.properties(d.propertyName)
        model
          .schemaToType(
            discriminatorProperty,
            child.requiredFields.contains(d.propertyName)
          )
          .map { discriminatorType =>
            val propName = Term.Name(d.propertyName)
            q"""sealed trait $traitName {
          def $propName: ${discriminatorType.tpe}
        }
        """
          }

      case None =>
        q"sealed trait $traitName".pure[IM]

    }
  }

  private def processParams(
      name: String,
      schema: SafeSchema,
      isRequired: Boolean
  ): IM[Term.Param] =
    model
      .schemaToType(
        schema,
        isRequired
      )
      .map(_.copy(paramName = name).asParam)
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
}
