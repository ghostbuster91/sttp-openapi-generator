package io.github.ghostbuster91.sttp.client3.openapi

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.Components

import scala.collection.JavaConverters._
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.PathItem
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.parameters._
import io.swagger.v3.oas.models.media._
import sttp.model.{
  MediaType => SttpMediaType,
  Method => SttpMethod,
  StatusCode => SttpStatusCode
}

class SafeOpenApi(openApi: OpenAPI) {
  def components: Option[SafeComponents] =
    Option(openApi.getComponents).map(new SafeComponents(_))
  def paths: Map[String, SafePathItem] =
    openApi.getPaths.asScala.toMap
      .mapValues(item => new SafePathItem(item))
      .toMap

  override def toString: String = openApi.toString
  def unsafe: OpenAPI = openApi
}

class SafeComponents(c: Components) {
  def schemas: Map[String, SafeSchema] =
    Option(c.getSchemas)
      .map(_.asScala.mapValues(SafeSchema.apply).toMap)
      .getOrElse(Map.empty)
  def requestBodies: Map[String, SafeRequestBody] =
    Option(c.getRequestBodies)
      .map(_.asScala.mapValues(new SafeRequestBody(_)).toMap)
      .getOrElse(Map.empty)
  override def toString: String = c.toString
  def unsafe: Components = c
}

case class SafeRequestBody(rb: RequestBody) {
  def content: Map[String, SafeMediaType] =
    Option(rb.getContent)
      .map(_.asScala.mapValues(new SafeMediaType(_)).toMap)
      .getOrElse(Map.empty)

  def required: Boolean = rb.getRequired()
  override def toString: String = rb.toString
}

case class SafePathItem(unsafe: PathItem) {
  def operations: Map[SttpMethod, SafeOperation] =
    Option(
      unsafe
        .readOperationsMap()
    )
      .map(
        _.asScala
          .map { case (m, o) =>
            SafePathItem.openapiMethodToSttp(m) -> SafeOperation(o)
          }
          .toMap
      )
      .getOrElse(Map.empty)

  override def toString: String = unsafe.toString
}
object SafePathItem {
  def openapiMethodToSttp(m: PathItem.HttpMethod): SttpMethod =
    m match {
      case PathItem.HttpMethod.POST    => SttpMethod.POST
      case PathItem.HttpMethod.GET     => SttpMethod.GET
      case PathItem.HttpMethod.PUT     => SttpMethod.PUT
      case PathItem.HttpMethod.PATCH   => SttpMethod.PATCH
      case PathItem.HttpMethod.DELETE  => SttpMethod.DELETE
      case PathItem.HttpMethod.HEAD    => SttpMethod.HEAD
      case PathItem.HttpMethod.OPTIONS => SttpMethod.OPTIONS
      case PathItem.HttpMethod.TRACE   => SttpMethod.TRACE
    }
}

case class SafeOperation(op: Operation) {
  def operationId: OperationId =
    OperationId(op.getOperationId)
  def tags: Option[List[String]] =
    Option(op.getTags).map(_.asScala.toList)

  def parameters: List[SafeParameter] =
    Option(op.getParameters)
      .map(_.asScala.toList.map {
        case pp: PathParameter   => SafePathParameter(pp)
        case cp: CookieParameter => SafeCookieParameter(cp)
        case hp: HeaderParameter => SafeHeaderParameter(hp)
        case qp: QueryParameter  => SafeQueryParameter(qp)
      })
      .getOrElse(List.empty)

  def responses: Map[SttpStatusCode, SafeApiResponse] =
    Option(op.getResponses)
      .map(
        _.asScala
          .collect {
            case (code, r) if code != "default" =>
              SttpStatusCode.unsafeApply(code.toInt) -> new SafeApiResponse(r)
            case ("default", r) =>
              SttpStatusCode.Ok -> new SafeApiResponse(r)
          }
          .toMap
      )
      .getOrElse(Map.empty)

  def requestBody: Option[SafeRequestBody] =
    Option(op.getRequestBody).map(new SafeRequestBody(_))

  def collectResponses(
      statusCodePredicate: SttpStatusCode => Boolean
  ): Map[SttpStatusCode, SafeSchema] = {
    val jsonMediaType = SttpMediaType.ApplicationJson.toString()
    responses
      .collect {
        case (statusCode, response) if statusCodePredicate(statusCode) =>
          response.content
            .get(jsonMediaType)
            .map(mt => statusCode -> mt.schema)
      }
      .flatten
      .toMap
  }

  override def toString: String = op.toString

  def unsafe: Operation = op
}

case class OperationId(v: String) extends AnyVal

sealed trait SafeParameter {
  def unsafe: Parameter
  def name: String = unsafe.getName
  def schema: SafeSchema = SafeSchema(unsafe.getSchema)
  def required: Boolean = unsafe.getRequired()
  override def toString: String = unsafe.toString
}
case class SafePathParameter(unsafe: PathParameter) extends SafeParameter
case class SafeHeaderParameter(unsafe: HeaderParameter) extends SafeParameter
case class SafeCookieParameter(unsafe: CookieParameter) extends SafeParameter
case class SafeQueryParameter(unsafe: QueryParameter) extends SafeParameter
class SafeApiResponse(r: ApiResponse) {
  def content: Map[String, SafeMediaType] =
    Option(r.getContent)
      .map(_.asScala.mapValues(v => new SafeMediaType(v)).toMap)
      .getOrElse(Map.empty)
}

class SafeMediaType(m: MediaType) {
  def schema: SafeSchema = SafeSchema(m.getSchema)
  def unsafe: MediaType = m
}

sealed trait SafeSchema {
  def unsafe: Schema[_]
  def enum: List[Any] =
    Option(unsafe.getEnum).map(_.asScala.toList).getOrElse(List.empty)
  def isEnum: Boolean = enum.nonEmpty
  def isArray = false
  override def toString: String = unsafe.toString
}
sealed trait SchemaWithProperties extends SafeSchema {
  def properties: Map[String, SafeSchema] = Option(unsafe.getProperties)
    .map(_.asScala.mapValues(SafeSchema.apply).toMap)
    .getOrElse(Map.empty)
  def requiredFields: List[String] =
    Option(unsafe.getRequired).map(_.asScala.toList).getOrElse(List.empty)
  def discriminator: Option[SafeDiscriminator] =
    Option(unsafe.getDiscriminator).map(SafeDiscriminator.apply)
}
sealed trait SafePrimitiveSchema extends SafeSchema
case class SafeArraySchema(unsafe: ArraySchema) extends SafeSchema {
  def items: SafeSchema = SafeSchema(unsafe.getItems)
  override def isArray = true
}
case class SafeBinarySchema(unsafe: BinarySchema) extends SafeSchema
case class SafeBooleanSchema(unsafe: BooleanSchema) extends SafeSchema {
  def default: Option[Boolean] = Option(unsafe.getDefault).map(_.booleanValue())
}
case class SafeByteArraySchema(unsafe: ByteArraySchema) extends SafeSchema
case class SafeDateSchema(unsafe: DateSchema) extends SafeSchema
case class SafeDateTimeSchema(unsafe: DateTimeSchema) extends SafeSchema
case class SafeEmailSchema(unsafe: EmailSchema) extends SafeSchema
case class SafeFileSchema(unsafe: FileSchema) extends SafeSchema
case class SafeIntegerSchema(unsafe: IntegerSchema) extends SafeSchema {
  def default: Option[Int] = Option(unsafe.getDefault).map(_.intValue())
}
case class SafeLongSchema(unsafe: IntegerSchema) extends SafeSchema {
  def default: Option[Long] = Option(unsafe.getDefault).map(_.longValue())
}
case class SafeMapSchema(unsafe: MapSchema) extends SchemaWithProperties {
  def additionalProperties: Either[Boolean, SafeSchema] =
    Option(unsafe.getAdditionalProperties)
      .map {
        case v: Schema[_] => Right(SafeSchema(v))
        case v            => Left(v.asInstanceOf[Boolean])
      }
      .getOrElse(Left(false))
}
case class SafeDoubleSchema(unsafe: NumberSchema) extends SafeSchema {
  def default: Option[Double] = Option(unsafe.getDefault).map(_.doubleValue())
}
case class SafeFloatSchema(unsafe: NumberSchema) extends SafeSchema {
  def default: Option[Float] = Option(unsafe.getDefault).map(_.floatValue())
}
case class SafeObjectSchema(unsafe: ObjectSchema) extends SchemaWithProperties
case class SafePasswordSchema(unsafe: PasswordSchema) extends SafeSchema
case class SafeStringSchema(unsafe: StringSchema) extends SafeSchema {
  def default: Option[String] = Option(unsafe.getDefault)
}
case class SafeUUIDSchema(unsafe: UUIDSchema) extends SafeSchema
case class SafeRefSchema(unsafe: Schema[_]) extends SafeSchema {
  def ref: SchemaRef = SchemaRef.parse(unsafe.get$ref)
}
case class SafeComposedSchema(unsafe: ComposedSchema) extends SafeSchema {
  def oneOf: List[SafeRefSchema] =
    Option(unsafe.getOneOf)
      .map(_.asScala.map(SafeRefSchema.apply).toList)
      .getOrElse(List.empty)

  def allOf: List[SafeSchema] =
    Option(unsafe.getAllOf)
      .map(_.asScala.map(SafeSchema(_)).toList)
      .getOrElse(List.empty)

  def discriminator: Option[SafeDiscriminator] =
    Option(unsafe.getDiscriminator).map(SafeDiscriminator.apply)
  override def toString: String = unsafe.toString
}

case class SafeDiscriminator(d: Discriminator) {
  def propertyName: String = d.getPropertyName
  def mapping: Map[String, SchemaRef] = Option(d.getMapping)
    .map(_.asScala.mapValues(SchemaRef.parse).toMap)
    .getOrElse(Map.empty)
}

object SafeSchema {
  def apply(s: Schema[_]): SafeSchema =
    s match {
      case as: ArraySchema            => SafeArraySchema(as)
      case bs: BooleanSchema          => SafeBooleanSchema(bs)
      case bas: ByteArraySchema       => SafeByteArraySchema(bas)
      case binarySchema: BinarySchema => SafeBinarySchema(binarySchema)
      case ds: DateSchema             => SafeDateSchema(ds)
      case dts: DateTimeSchema        => SafeDateTimeSchema(dts)
      case es: EmailSchema            => SafeEmailSchema(es)
      case fs: FileSchema             => SafeFileSchema(fs)
      case is: IntegerSchema if Option(is.getFormat).contains("int64") =>
        SafeLongSchema(is)
      case is: IntegerSchema => SafeIntegerSchema(is)
      case ms: MapSchema     => SafeMapSchema(ms)
      case ns: NumberSchema if Option(ns.getFormat).contains("float") =>
        SafeFloatSchema(ns)
      case ns: NumberSchema               => SafeDoubleSchema(ns)
      case os: ObjectSchema               => SafeObjectSchema(os)
      case ps: PasswordSchema             => SafePasswordSchema(ps)
      case ss: StringSchema               => SafeStringSchema(ss)
      case us: UUIDSchema                 => SafeUUIDSchema(us)
      case other if other.get$ref != null => SafeRefSchema(other)
      case composed: ComposedSchema       => SafeComposedSchema(composed)
    }
}

sealed trait SchemaRef {
  def key: String
  def ref: String
}
object SchemaRef {
  def schema(key: String): SchemaRef = SchemaRef.Schema(key)
  def requestBody(key: String): SchemaRef = SchemaRef.RequestBody(key)

  case class Schema(key: String) extends SchemaRef {
    def ref: String = s"#/components/schemas/$key"
  }
  case class RequestBody(key: String) extends SchemaRef {
    def ref: String = s"#/components/requestBodies/$key"
  }

  def parse(ref: String): SchemaRef =
    if (ref.contains("#/components/schemas/")) {
      SchemaRef.Schema(ref.replaceAll("#/components/schemas/", ""))
    } else if (ref.contains("#/components/requestBodies/")) {
      SchemaRef.RequestBody(ref.replaceAll("#/components/requestBodies/", ""))
    } else {
      SchemaRef.Schema(ref)
    }
}
case class StatusCodeResponse(schema: SafeSchema, statusCode: Int)
