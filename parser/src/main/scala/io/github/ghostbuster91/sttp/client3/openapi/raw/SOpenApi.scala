package io.github.ghostbuster91.sttp.client3.openapi.raw

import GOpenapiSchema._

case class RReferenceObject()

//TODO add links and callbacks
case class GComponents[C[_]](
    schemas: C[Map[String, GSchemaObject[C]]],
    responses: C[Map[String, GReferenceOr[C, RResponseObject[C]]]],
    paramteres: C[Map[String, GReferenceOr[C, GParameter[C]]]],
    examples: C[Map[String, GReferenceOr[C, RExampleObject[C]]]],
    requestBodies: C[Map[String, GReferenceOr[C, RRequestBodyObject[C]]]],
    headers: C[Map[String, GReferenceOr[C, RHeaderObject[C]]]],
    securitySchemas: C[Map[String, GReferenceOr[C, RSecuritySchemaObject[C]]]],
    paths: C[Map[String, GReferenceOr[C, GPathItem[C]]]]
)

case class RResponseObject[C[_]]()
case class RExampleObject[C[_]]()
case class RRequestBodyObject[C[_]]()
case class RHeaderObject[C[_]]()
case class RSecuritySchemaObject[C[_]]()

case class GPathItem[C[_]](
    $ref: Option[String],
    summary: Option[String],
    description: Option[String],
    get: Option[GOperation[C]],
    put: Option[GOperation[C]],
    post: Option[GOperation[C]],
    delete: Option[GOperation[C]],
    options: Option[GOperation[C]],
    head: Option[GOperation[C]],
    patch: Option[GOperation[C]],
    trace: Option[GOperation[C]],
    servers: Option[List[GServer[C]]],
    parameters: Option[List[Either[GParameter[C], GReference[C]]]]
)

case class GServer[C[_]](
    url: C[String]
)

case class GParameter[C[_]](
    name: Option[String],
    in: Option[String],
    description: Option[String],
    required: Option[Boolean],
    deprecated: Option[Boolean],
    allowEmptyValue: Option[Boolean],
    style: Option[String],
    explode: Option[Boolean],
    allowReserved: Option[Boolean],
    schema: Option[Int],
    example: Option[Any],
    examples: Option[Map[String, GExample]],
    content: Option[Map[String, GMediaType]]
)

case class GReference[C[_]](
    $ref: C[String]
)

case class GResponse(
    description: Option[String]
)

case class GOperation[C[_]](
    tags: C[List[String]],
    summary: Option[String],
    description: Option[String],
    externalDocs: Option[GExternalDocumentation],
    operationId: Option[String],
    parameters: Option[List[Either[GParameter[C], GReference[C]]]],
    requestBody: Option[Either[MRequestBody, GReference[C]]],
    responses: Option[GResponses[C]],
    callbacks: Option[Map[String, Either[GCallback[C], GReference[C]]]],
    deprecated: Option[Boolean],
    security: Option[List[GSecurityRequirement]],
    servers: Option[List[GServer[C]]]
)

case class GExternalDocumentation(
    url: Option[String],
    description: Option[String]
)

case class MRequestBody(
    description: Option[String],
    content: Map[String, GMediaType],
    required: Option[Boolean]
)

case class GMediaType(
    schema: Option[Int]
)

case class GResponses[C[_]](
    default: Option[Either[GResponse, GReference[C]]],
    statusCodes: Option[Map[String, Either[GResponse, GReference[C]]]]
)

case class GCallback[C[_]](
    expression: Option[String],
    callbacks: Option[Map[String, Either[GPathItem[C], GReference[C]]]]
)

case class GSecurityRequirement(
    name: Option[String],
    scopes: Option[List[String]]
)

case class GExample()

// copied from https://github.com/softwaremill/tapir/blob/6adf13da6686b2fac2685b8e1e02efe1e07e7aa8/openapi-codegen/core/src/main/scala/sttp/tapir/codegen/openapi/models/OpenapiSchemaType.scala#L3
sealed trait GOpenapiSchema {
  def nullable: Boolean
}

object GOpenapiSchema {
  sealed trait OpenapiSchemaMixedType extends GOpenapiSchema
  sealed trait OpenapiSchemaSimpleType extends GOpenapiSchema

  // https://swagger.io/docs/specification/data-models/oneof-anyof-allof-not/
  case class OpenapiSchemaOneOf(
      types: Seq[OpenapiSchemaSimpleType]
  ) extends OpenapiSchemaMixedType {
    val nullable: Boolean = false
  }

  case class OpenapiSchemaAnyOf(
      types: Seq[OpenapiSchemaSimpleType]
  ) extends OpenapiSchemaMixedType {
    val nullable: Boolean = false
  }

  case class OpenapiSchemaAllOf(
      types: Seq[OpenapiSchemaSimpleType]
  ) extends OpenapiSchemaMixedType {
    val nullable: Boolean = false
  }

  case class OpenapiSchemaNot(
      `type`: GOpenapiSchema
  ) extends GOpenapiSchema {
    val nullable: Boolean = false
  }

  // https://swagger.io/docs/specification/data-models/data-types/#numbers
  // no min/max, exclusiveMin/exclusiveMax, multipleOf support
  sealed trait OpenapiSchemaNumericType extends OpenapiSchemaSimpleType

  case class OpenapiSchemaDouble(
      nullable: Boolean
  ) extends OpenapiSchemaNumericType
  case class OpenapiSchemaFloat(
      nullable: Boolean
  ) extends OpenapiSchemaNumericType
  case class OpenapiSchemaLong(
      nullable: Boolean
  ) extends OpenapiSchemaNumericType
  case class OpenapiSchemaInt(
      nullable: Boolean
  ) extends OpenapiSchemaNumericType

  // https://swagger.io/docs/specification/data-models/data-types/#string
  // no minLength/maxLength, pattern support
  sealed trait OpenapiSchemaStringType extends OpenapiSchemaSimpleType

  case class OpenapiSchemaString(
      nullable: Boolean
  ) extends OpenapiSchemaStringType
  case class OpenapiSchemaDate(
      nullable: Boolean
  ) extends OpenapiSchemaStringType
  case class OpenapiSchemaDateTime(
      nullable: Boolean
  ) extends OpenapiSchemaStringType
  case class OpenapiSchemaByte(
      nullable: Boolean
  ) extends OpenapiSchemaStringType
  case class OpenapiSchemaBinary(
      nullable: Boolean
  ) extends OpenapiSchemaStringType

  case class OpenapiSchemaBoolean(
      nullable: Boolean
  ) extends OpenapiSchemaSimpleType

  case class OpenapiSchemaRef(
      name: String
  ) extends OpenapiSchemaSimpleType {
    val nullable = false
  }

  // no minItems/maxItems, uniqueItems support
  case class OpenapiSchemaArray(
      items: GOpenapiSchema,
      nullable: Boolean
  ) extends GOpenapiSchema

  // no readOnly/writeOnly, minProperties/maxProperties support
  case class GSchemaObject[C[_]](
      properties: Map[String, GOpenapiSchema],
      required: Seq[String],
      nullable: Boolean
  ) extends GOpenapiSchema
}
