package io.github.ghostbuster91.sttp.client3

import cats.data.NonEmptyList
import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.model.{
  ClassName,
  ParameterName,
  ParameterRef
}
import io.github.ghostbuster91.sttp.client3.openapi._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._

import scala.meta._
import _root_.io.github.ghostbuster91.sttp.client3.openapi.zz.OpenapiSchemaType._
import _root_.io.github.ghostbuster91.sttp.client3.openapi.zz.OpenapiSchemaType

case class Model(
    schemas: Map[OpenapiSchemaRef, OpenapiSchemaType],
    classNames: Map[OpenapiSchemaRef, ClassName],
    childToParentRef: Map[OpenapiSchemaRef, List[OpenapiSchemaRef]],
    typeMappings: TypesMapping
) {

  def classNameFor(schemaRef: OpenapiSchemaRef): ClassName = classNames(
    schemaRef
  )
  def schemaFor(schemaRef: OpenapiSchemaRef): OpenapiSchemaType = schemas(
    schemaRef
  )

  def schemaToType(
      schema: OpenapiSchemaType
  ): IM[Type] = {
    val declType = schemaToParameter(schema)
    declType.map(d => d.tpe)
  }

  def schemaToParameter(
      key: String,
      schema: OpenapiSchemaType,
      isRequired: Boolean
  ): IM[ParameterRef] =
    schemaToParameter(schema, isRequired).map(_.withName(key))

  def schemaToParameter(
      schema: OpenapiSchemaType,
      isRequired: Boolean
  ): IM[ParameterRef] = {
    val declType = schemaToParameter(schema)
    if (isRequired || schema.isInstanceOf[OpenapiSchemaArray]) {
      declType
    } else {
      declType.map(_.asOption)
    }
  }

  private def schemaToParameter(schema: OpenapiSchemaType): IM[ParameterRef] = {
    schema match {
      case OpenapiSchemaObject(properties, required, nullable) => ???
      case OpenapiSchemaBoolean(nullable) =>
        ParameterRef("Boolean", none).pure[IM]
      case OpenapiSchemaDouble(nullable) =>
        ParameterRef("Double", None).pure[IM]
      case OpenapiSchemaFloat(nullable) =>
        ParameterRef("Float", None).pure[IM]
      case OpenapiSchemaInt(nullable) =>
        ParameterRef("Int", None).pure[IM]
      case OpenapiSchemaLong(nullable) =>
        ParameterRef("Long", None).pure[IM]
      case ref:OpenapiSchemaRef =>
        ParameterRef(classNames(ref).v, None).pure[IM]
      case OpenapiSchemaByte(nullable) => ???
      case OpenapiSchemaDate(nullable) =>

        val mappingType = s"_root_.${typeMappings.date.getName}"
        val importer = Import(List(mappingType.parse[Importer].get))

        registerExternalTpe(importer).map(tpe =>
          ParameterRef(tpe, ParameterName("date"), None)
        )
      case OpenapiSchemaDateTime(nullable) =>

        val mappingType = s"_root_.${typeMappings.dateTime.getName}"
        val importer = Import(List(mappingType.parse[Importer].get))

        registerExternalTpe(importer).map(tpe =>
          ParameterRef(tpe, ParameterName("dateTime"), None)
        )
      case OpenapiSchemaBinary(nullable) => ???
      case OpenapiSchemaString(nullable) =>
        ParameterRef("String", None).pure[IM]
      case OpenapiSchemaArray(items, nullable) =>
        schemaToParameter(items).map { itemTypeRef =>
          ParameterRef(
            t"List[${itemTypeRef.tpe}]",
            ParameterName(itemTypeRef.paramName.v + "List"),
            None
          )
        }
      case OpenapiSchemaAllOf(types) => ???
      case OpenapiSchemaAnyOf(types) => ???
      case OpenapiSchemaOneOf(types) => ???
      case OpenapiSchemaNot(_) => ???
    }
  }

  def commonAncestor(
      childs: NonEmptyList[OpenapiSchemaRef]
  ): List[OpenapiSchemaRef] =
    childs
      .map(c => childToParentRef.getOrElse(c, List(c)))
      .map(_.toSet)
      .reduce[Set[OpenapiSchemaRef]](_ intersect _)
      .toList
}

object Model {
  def apply(
      schemas: Map[String, OpenapiSchemaType],
      requestBodies: Map[String, OpenapiSchemaType],
      typeMappings: TypesMapping
  ): Model = {
    val adjSchemas = schemas.map { case (k, v) =>
      OpenapiSchemaRef(s"#/components/schemas/$k") -> v
    }
    val adjReqBodies = requestBodies.map { case (k, v) =>
      OpenapiSchemaRef(s"#/components/requestBodies/$k") -> v
    }

    val refToSchema = adjSchemas ++ adjReqBodies
    val modelClassNames =
      refToSchema.keys
        .map(key => key -> ClassName(snakeToCamelCase(key.name)))
        .toMap
    val childToParentRef = calculateChildToParent(refToSchema)
    new Model(
      refToSchema,
      modelClassNames,
      childToParentRef.toMap,
      typeMappings
    )
  }

  private def calculateChildToParent(
      refToSchema: Map[OpenapiSchemaRef, OpenapiSchemaType]
  ) =
    refToSchema
      .collect {
        case (key, composed: OpenapiSchemaAllOf) =>
          composed.types.collect { case p: OpenapiSchemaRef => key -> p }
        case (key, composed: OpenapiSchemaAnyOf) =>
          composed.types.collect { case p: OpenapiSchemaRef => key -> p }
        case (key, composed: OpenapiSchemaOneOf) =>
          composed.types.collect { case p: OpenapiSchemaRef => key -> p }
      }
      .flatten
      .groupBy(_._1)
      .mapValues(e => e.map(_._2).toList)

  private def snakeToCamelCase(snake: String) =
    snake.split('_').toList.map(_.capitalize).mkString
}
