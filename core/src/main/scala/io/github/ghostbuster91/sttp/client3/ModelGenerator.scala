package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.json._
import io.github.ghostbuster91.sttp.client3.openapi._
import scala.meta._

class ModelGenerator(
    schemas: Map[SchemaRef, SafeSchema],
    classNames: Map[SchemaRef, String],
    ir: ImportRegistry,
    jsonTypeProvider: JsonTypeProvider
) {
  def generate: Map[SchemaRef, Defn] = {
    val childToParentRef: Map[SchemaRef, List[SchemaRef]] = schemas
      .collect { case (key, composed: SafeComposedSchema) =>
        composed.oneOf.map(c => c.ref -> key)
      }
      .flatten
      .groupBy(_._1)
      .mapValues(e => e.map(_._2).toList)
    val parentToChilds = schemas.collect {
      case (key, composed: SafeComposedSchema) =>
        key -> composed.oneOf.map(_.ref)
    }.toMap

    val classes = schemas.collect { case (key, schema: SchemaWithProperties) =>
      key -> schemaToClassDef(
        classNames(key),
        schema,
        childToParentRef.getOrElse(key, List.empty).map(classNames.apply)
      )
    }
    val traits = schemas.collect { case (key, composed: SafeComposedSchema) =>
      key -> schemaToSealedTrait(
        classNames(key),
        composed.discriminator,
        parentToChilds(key)
          .map(c => schemas(c).asInstanceOf[SafeObjectSchema])
      )
    }
    traits ++ classes
  }

  def classNameFor(schemaRef: SchemaRef): ClassName = ClassName(
    classNames(schemaRef)
  )
  def schemaFor(schemaRef: SchemaRef): SafeSchema = schemas(schemaRef)

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
        val discriminatorType = schemaToType(
          discriminatorProperty,
          child.requiredFields.contains(d.propertyName)
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
    val declType = schemaToType(
      schema,
      isRequired
    )
    declType.copy(paramName = name).asParam
  }

  def schemaToType(
      schema: SafeSchema,
      isRequired: Boolean
  ): TypeRef = {
    val declType = schemaToType(schema)
    ModelGenerator.optionApplication(declType, isRequired, schema.isArray)
  }

  private def schemaToType(schema: SafeSchema): TypeRef =
    schema match {
      case ss: SafeStringSchema =>
        TypeRef("String", ss.default.map(Lit.String(_)))
      case si: SafeIntegerSchema =>
        TypeRef("Int", si.default.map(Lit.Int(_)))
      case sl: SafeLongSchema =>
        TypeRef("Long", sl.default.map(Lit.Long(_)))
      case sf: SafeFloatSchema =>
        TypeRef("Float", sf.default.map(Lit.Float(_)))
      case sd: SafeDoubleSchema =>
        TypeRef("Double", sd.default.map(Lit.Double(_)))
      case sb: SafeBooleanSchema =>
        TypeRef("Boolean", sb.default.map(Lit.Boolean(_)))
      case s: SafeArraySchema =>
        val itemTypeRef = schemaToType(s.items)
        TypeRef(
          t"List[${itemTypeRef.tpe}]",
          itemTypeRef.paramName + "List",
          None
        )
      case ref: SafeRefSchema =>
        TypeRef(classNames(ref.ref), None)
      case _: SafeUUIDSchema =>
        ir.registerImport(q"import _root_.java.util.UUID")
        TypeRef(t"UUID", "uuid", None)
    }
}

object ModelGenerator {
  def apply(
      schemas: Map[String, SafeSchema],
      requestBodies: Map[String, SafeSchema],
      ir: ImportRegistry,
      jsonTypeProvider: JsonTypeProvider
  ): ModelGenerator = {
    val modelClassNames = schemas.map { case (key, _) =>
      SchemaRef.schema(key) -> snakeToCamelCase(key)
    } ++ requestBodies.map { case (key, _) =>
      SchemaRef.requestBody(key) -> snakeToCamelCase(key)
    }
    new ModelGenerator(
      schemas.map { case (k, v) =>
        SchemaRef.schema(k) -> v
      } ++ requestBodies
        .map { case (k, v) => SchemaRef.requestBody(k) -> v },
      modelClassNames,
      ir,
      jsonTypeProvider
    )
  }

  private def snakeToCamelCase(snake: String) =
    snake.split('_').toList.map(_.capitalize).mkString

  def optionApplication(
      declType: TypeRef,
      isRequired: Boolean,
      isCollection: Boolean
  ): TypeRef =
    if (isRequired || isCollection) {
      declType
    } else {
      declType.copy(
        tpe = t"Option[${declType.tpe}]",
        defaultValue = declType.defaultValue.map(d => q"Some($d)")
      )
    }
}
