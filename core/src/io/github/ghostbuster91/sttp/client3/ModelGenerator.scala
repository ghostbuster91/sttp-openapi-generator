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
    val parentToChilds = schemas.collect {
      case (key, composed: SafeComposedSchema) =>
        key -> composed.oneOf.map(_.ref)
    }.toMap

    val classes = schemas.collect { case (key, schema: SchemaWithProperties) =>
      key -> schemaToClassDef(
        classNames(key),
        schema,
        childToParentRef.get(key).map(classNames.apply),
      )
    }
    val traits = schemas.collect { case (key, composed: SafeComposedSchema) =>
      key -> schemaToSealedTrait(
        classNames(key),
        composed.discriminator,
        parentToChilds(key)
          .map(c => c -> schemas(c).asInstanceOf[SafeObjectSchema]),
      )
    }
    traits ++ classes
  }
  def classNameFor(schemaRef: SchemaRef): String = classNames(schemaRef)

  private def schemaToClassDef(
      name: String,
      schema: SchemaWithProperties,
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
    schema match {
      case _: SafeMapSchema =>
        parentClassName match {
          case Some(value) =>
            val parentTypeName = Type.Name(value)
            val parentInit = init"$parentTypeName()"
            q"""case class $className(..$props) extends Map[String,Any] with $parentInit{
                private val _internal_map = Map.empty[String,Any]
                override def removed(key: String): Map[String, Any] = _internal_map.removed(key)
                override def updated[V1 >: Any](key: String, value: V1): Map[String, V1] = _internal_map.updated(key,value)
                override def get(key: String): Option[Any] = _internal_map.get(key)
                override def iterator: Iterator[(String, Any)] = _internal_map.iterator
            }"""
          case None =>
            q"""case class $className(..$props) extends Map[String, Any] {
                private val _internal_map = Map.empty[String,Any]
                override def removed(key: String): Map[String, Any] = _internal_map.removed(key)
                override def updated[V1 >: Any](key: String, value: V1): Map[String, V1] = _internal_map.updated(key,value)
                override def get(key: String): Option[Any] = _internal_map.get(key)
                override def iterator: Iterator[(String, Any)] = _internal_map.iterator
            }"""
        }
      case _: SafeObjectSchema =>
        parentClassName match {
          case Some(value) =>
            val parentTypeName = Type.Name(value)
            val parentInit = init"$parentTypeName()"
            q"case class $className(..$props) extends $parentInit{}"
          case None =>
            q"case class $className(..$props)"
        }
    }
  }

  private def schemaToSealedTrait(
      name: String,
      discriminator: Option[SafeDiscriminator],
      childs: List[(SchemaRef, SafeObjectSchema)],
  ): Defn.Trait = {
    val traitName = Type.Name(name)
    discriminator match {
      case Some(d) =>
        val (childRef, child) = childs.head
        val discriminatorProperty = child.properties(d.propertyName)
        val discriminatorType = schemaToType(
          s"${childRef.key}${d.propertyName}",
          discriminatorProperty,
          child.requiredFields.contains(d.propertyName),
        )
        val propName = Term.Name(d.propertyName)
        q"""sealed trait $traitName {
          def $propName: $discriminatorType
        }
        """
      case None =>
        q"sealed trait $traitName"

    }
  }

  private def processParams(
      className: String,
      name: String,
      schema: SafeSchema,
      isRequired: Boolean,
  ): Term.Param = {
    val declType = schemaToType(
      s"${className.capitalize}${name.capitalize}", //TODO className shouldn't be needed here as enums have to be unique globally
      schema,
      isRequired,
    )
    paramDeclFromType(name, declType)
  }

  def schemaToType(
      enumName: String,
      schema: SafeSchema,
      isRequired: Boolean,
  ): Type = {
    val declType = schemaToType(enumName, schema)
    ModelGenerator.optionApplication(declType, isRequired)
  }

  private def schemaToType(enumName: String, schema: SafeSchema): Type =
    schema match {
      case ss: SafeStringSchema =>
        if (ss.isEnum) {
          Type.Name(enumName)
        } else {
          t"String"
        }
      case si: SafeIntegerSchema =>
        if (si.isEnum) {
          Type.Name(enumName)
        } else {
          si.format match {
            case Some("int64") => t"Long"
            case _             => t"Int"
          }
        }
      case sn: SafeNumberSchema =>
        sn.format match {
          case Some("float") => t"Float"
          case _             => t"Double"
        }
      case s: SafeArraySchema =>
        t"List[${schemaToType(enumName, s.items)}]"
      case ref: SafeRefSchema =>
        Type.Name(classNames(ref.ref))
      case _: SafeUUIDSchema =>
        t"UUID"
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
      schemas.map { case (k, v) =>
        SchemaRef.schema(k) -> v
      } ++ requestBodies
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
