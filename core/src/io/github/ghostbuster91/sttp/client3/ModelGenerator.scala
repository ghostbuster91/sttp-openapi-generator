package io.github.ghostbuster91.sttp.client3

import scala.meta._

class ModelGenerator(
    schemas: Map[SchemaRef, SafeSchema],
    classNames: Map[SchemaRef, String],
) {
  def generate: Map[SchemaRef, Defn] = {
    val childToParentRef = schemas
      .collect { case (key, composed: SafeComposedSchema) =>
        composed.oneOf.map(c => c.ref -> key)
      }
      .flatten
      .toMap

    schemas.collect {
      case (key, schema: SafeObjectSchema) =>
        key -> schemaToClassDef(
          classNames(key),
          schema,
          childToParentRef.get(key).map(classNames.apply),
        )
      case (key, _: SafeComposedSchema) =>
        key -> schemaToSealedTrait(classNames(key))
    }
  }
  def classNameFor(schemaRef: SchemaRef): String = classNames(schemaRef)

  private def schemaToClassDef(
      name: String,
      schema: SafeObjectSchema,
      parentClassName: Option[String],
  ) = {
    val props = schema.properties.map { case (k, v) =>
      processParams(
        name,
        k,
        v,
        schema.requiredFields.contains(k),
      )
    }.toList
    val className = Type.Name(name)
    parentClassName match {
      case Some(value) =>
        val parentTypeName = Type.Name(value)
        val parentInit = init"$parentTypeName()"
        q"case class $className(..$props) extends $parentInit{}"
      case None =>
        q"case class $className(..$props)"
    }
  }

  private def schemaToSealedTrait(
      name: String,
  ): Defn.Trait = {
    val traitName = Type.Name(name)
    q"sealed trait $traitName"
  }

  private def processParams(
      className: String,
      name: String,
      schema: SafeSchema,
      isRequired: Boolean,
  ): Term.Param = {
    val declType = schemaToType(className, name, schema, isRequired)
    paramDeclFromType(name, declType)
  }

  def schemaToType(
      className: String,
      propertyName: String,
      schema: SafeSchema,
      isRequired: Boolean,
  ): Type = {
    val declType = schemaToType(className, propertyName, schema)
    ModelGenerator.optionApplication(declType, isRequired)
  }

  private def schemaToType(
      className: String,
      propertyName: String,
      schema: SafeSchema,
  ): Type =
    schema match {
      case ss: SafeStringSchema =>
        if (ss.isEnum) {
          Type.Name(s"${className.capitalize}${propertyName.capitalize}")
        } else {
          t"String"
        }
      case si: SafeIntegerSchema =>
        if (si.isEnum) {
          Type.Name(s"${className.capitalize}${propertyName.capitalize}")
        } else {
          t"Int"
        }
      case s: SafeArraySchema =>
        t"List[${schemaToType(className, propertyName, s.items)}]"
      case ref: SafeRefSchema =>
        Type.Name(classNames(ref.ref))
    }

  private def paramDeclFromType(paramName: String, declType: Type) = {
    val tpeName = Term.Name(paramName)
    param"$tpeName: $declType"
  }
}

object ModelGenerator {
  def apply(
      schemas: Map[String, SafeSchema],
      requestBodies: Map[String, SafeSchema],
  ): ModelGenerator = {
    val modelClassNames = schemas.map { case (key, _) =>
      SchemaRef.schema(key) -> snakeToCamelCase(key)
    } ++ requestBodies.map { case (key, _) =>
      SchemaRef.requestBody(key) -> snakeToCamelCase(key)
    }
    new ModelGenerator(
      schemas.map { case (k, v) => SchemaRef.schema(k) -> v } ++ requestBodies
        .map { case (k, v) => SchemaRef.requestBody(k) -> v },
      modelClassNames,
    )
  }

  private def snakeToCamelCase(snake: String) =
    snake.split('_').toList.map(_.capitalize).mkString

  def optionApplication(declType: Type, isRequired: Boolean): Type =
    if (isRequired) {
      declType
    } else {
      t"Option[$declType]"
    }
}
