package io.github.ghostbuster91.sttp.client3

import cats.data.NonEmptyList
import io.github.ghostbuster91.sttp.client3.model.{ClassName, TypeRef}
import io.github.ghostbuster91.sttp.client3.openapi._

import scala.meta._

case class Model(
    schemas: Map[SchemaRef, SafeSchema],
    classNames: Map[SchemaRef, String],
    childToParentRef: Map[SchemaRef, List[SchemaRef]]
) {

  def classNameFor(schemaRef: SchemaRef): ClassName = ClassName(
    classNames(schemaRef)
  )
  def schemaFor(schemaRef: SchemaRef): SafeSchema = schemas(schemaRef)

  def schemaToType(
      schema: SafeSchema,
      isRequired: Boolean,
      ir: ImportRegistry
  ): TypeRef = {
    val declType = schemaToType(schema, ir)
    if (isRequired || schema.isArray) {
      declType
    } else {
      declType.asOption
    }
  }

  private def schemaToType(schema: SafeSchema, ir: ImportRegistry): TypeRef =
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
        val itemTypeRef = schemaToType(s.items, ir)
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

  def commonAncestor(childs: NonEmptyList[SchemaRef]): List[SchemaRef] =
    childs
      .map(c => childToParentRef.getOrElse(c, List(c)))
      .map(_.toSet)
      .reduce[Set[SchemaRef]](_ intersect _)
      .toList
}

object Model {
  def apply(
      schemas: Map[String, SafeSchema],
      requestBodies: Map[String, SafeSchema]
  ): Model = {
    val adjSchemas = schemas.map { case (k, v) => SchemaRef.schema(k) -> v }
    val adjReqBodies = requestBodies.map { case (k, v) =>
      SchemaRef.requestBody(k) -> v
    }

    val refToSchema = adjSchemas ++ adjReqBodies
    val modelClassNames =
      refToSchema.keys.map(key => key -> snakeToCamelCase(key.key)).toMap
    val childToParentRef = calculateChildToParent(refToSchema)
    new Model(
      refToSchema,
      modelClassNames,
      childToParentRef
    )
  }

  private def calculateChildToParent(refToSchema: Map[SchemaRef, SafeSchema]) =
    refToSchema
      .collect { case (key, composed: SafeComposedSchema) =>
        composed.oneOf.map(c => c.ref -> key)
      }
      .flatten
      .groupBy(_._1)
      .mapValues(e => e.map(_._2).toList)

  private def snakeToCamelCase(snake: String) =
    snake.split('_').toList.map(_.capitalize).mkString
}
