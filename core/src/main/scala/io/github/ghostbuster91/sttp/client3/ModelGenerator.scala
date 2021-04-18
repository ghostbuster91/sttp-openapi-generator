package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.json._
import io.github.ghostbuster91.sttp.client3.openapi._
import scala.meta._

class ModelGenerator(
    model: Model,
    ir: ImportRegistry,
    jsonTypeProvider: JsonTypeProvider
) {
  def generate: Map[SchemaRef, Defn] = {
    val parentToChilds = model.schemas.collect {
      case (key, composed: SafeComposedSchema) =>
        key -> composed.oneOf.map(_.ref)
    }

    val classes = model.schemas.collect {
      case (key, schema: SchemaWithProperties) =>
        key -> schemaToClassDef(
          model.classNames(key),
          schema,
          model.childToParentRef
            .getOrElse(key, List.empty)
            .map(model.classNames.apply)
        )
    }
    val traits = model.schemas.collect {
      case (key, composed: SafeComposedSchema) =>
        key -> schemaToSealedTrait(
          model.classNames(key),
          composed.discriminator,
          parentToChilds(key)
            .map(c => model.schemas(c).asInstanceOf[SafeObjectSchema])
        )
    }
    traits ++ classes
  }

  private def schemaToClassDef(
      name: String,
      schema: SchemaWithProperties,
      parentClassName: List[String]
  ) = {
    val props = schema.properties.map { case (k, v) =>
      processParams(
        k,
        v,
        schema.requiredFields.contains(k)
      )
    }.toList
    val className = Type.Name(name)
    val adjustedProps = schema match {
      case _: SafeMapSchema =>
        val anyType = jsonTypeProvider.anyType
        props :+ param"_additionalProperties: Map[String, $anyType]"
      case _: SafeObjectSchema => props
    }
    parentClassName match {
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
  ): Defn.Trait = {
    val traitName = Type.Name(name)
    discriminator match {
      case Some(d) =>
        val child = childs.head
        val discriminatorProperty = child.properties(d.propertyName)
        val discriminatorType = model.schemaToType(
          discriminatorProperty,
          child.requiredFields.contains(d.propertyName),
          ir
        )
        val propName = Term.Name(d.propertyName)
        q"""sealed trait $traitName {
          def $propName: ${discriminatorType.tpe}
        }
        """
      case None =>
        q"sealed trait $traitName"

    }
  }

  private def processParams(
      name: String,
      schema: SafeSchema,
      isRequired: Boolean
  ): Term.Param = {
    val declType = model.schemaToType(
      schema,
      isRequired,
      ir
    )
    declType.copy(paramName = name).asParam
  }

}

object ModelGenerator {
  def apply(
      model: Model,
      ir: ImportRegistry,
      jsonTypeProvider: JsonTypeProvider
  ): ModelGenerator =
    new ModelGenerator(
      model,
      ir,
      jsonTypeProvider
    )
}
