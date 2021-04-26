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

  override def toString: String = openApi.toString
  private[openapi] def unsafe: OpenAPI = openApi
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
  private[openapi] def unsafe: Components = c
}

class SafeRequestBody(rb: RequestBody) {
  def content: Map[String, SafeMediaType] =
    Option(rb.getContent)
      .map(_.asScala.mapValues(new SafeMediaType(_)).toMap)
      .getOrElse(Map.empty)

  def required: Boolean = rb.getRequired()
  override def toString: String = rb.toString
}

class SafePathItem(p: PathItem) {
  def operations: Map[SttpMethod, SafeOperation] =
    List(
      Option(p.getGet).map(op =>
        (SttpMethod.GET: SttpMethod) -> new SafeOperation(op)
      ),
      Option(p.getPut).map(op =>
        (SttpMethod.PUT: SttpMethod) -> new SafeOperation(op)
      ),
      Option(p.getPost).map(op =>
        (SttpMethod.POST: SttpMethod) -> new SafeOperation(op)
      )
    ).flatten.toMap
  override def toString: String = p.toString
}

class SafeOperation(op: Operation) {
  def operationId: String =
    op.getOperationId //TODO introduce operationId value object
  def tags: Option[List[String]] =
    Option(op.getTags).map(_.asScala.toList)

  def parameters: List[SafeParameter] =
    Option(op.getParameters)
      .map(_.asScala.toList.map {
        case pp: PathParameter   => new SafePathParameter(pp)
        case cp: CookieParameter => new SafeCookieParameter(cp)
        case hp: HeaderParameter => new SafeHeaderParameter(hp)
        case qp: QueryParameter  => new SafeQueryParameter(qp)
      })
      .getOrElse(List.empty)

  def responses: Map[String, SafeApiResponse] =
    Option(op.getResponses)
      .map(_.asScala.mapValues(r => new SafeApiResponse(r)).toMap)
      .getOrElse(Map.empty)

  def requestBody: Option[SafeRequestBody] =
    Option(op.getRequestBody).map(new SafeRequestBody(_))

  def collectResponses(
      statusCodePredicate: SttpStatusCode => Boolean
  ): Map[Int, SafeSchema] = {
    val jsonMediaType = SttpMediaType.ApplicationJson.toString()
    responses
      .collect {
        case (statusCode, response)
            if statusCodePredicate(
              SttpStatusCode.unsafeApply(statusCode.toInt)
            ) =>
          response.content
            .get(jsonMediaType)
            .map(mt => statusCode.toInt -> mt.schema)
      }
      .flatten
      .toMap
  }

  override def toString: String = op.toString
}

sealed abstract class SafeParameter(p: Parameter) {
  def name: String = p.getName
  def schema: SafeSchema = SafeSchema(p.getSchema)
  def required: Boolean = p.getRequired()
  override def toString: String = p.toString
  private[openapi] def unsafe: Parameter = p
}
class SafePathParameter(p: PathParameter) extends SafeParameter(p)
class SafeHeaderParameter(p: HeaderParameter) extends SafeParameter(p)
class SafeCookieParameter(p: CookieParameter) extends SafeParameter(p)
class SafeQueryParameter(p: QueryParameter) extends SafeParameter(p)
class SafeApiResponse(r: ApiResponse) {
  def content: Map[String, SafeMediaType] =
    Option(r.getContent)
      .map(_.asScala.mapValues(v => new SafeMediaType(v)).toMap)
      .getOrElse(Map.empty)
}

class SafeMediaType(m: MediaType) {
  def schema: SafeSchema = SafeSchema(m.getSchema)
  private[openapi] def unsafe: MediaType = m
}

sealed abstract class SafeSchema(s: Schema[_]) {
  def enum: List[Any] =
    Option(s.getEnum).map(_.asScala.toList).getOrElse(List.empty)
  def isEnum: Boolean = enum.nonEmpty
  def isArray = false
  override def toString: String = s.toString
  private[openapi] def unsafe: Schema[_] = s
}
sealed abstract class SchemaWithProperties(s: Schema[_]) extends SafeSchema(s) {
  def properties: Map[String, SafeSchema] = Option(s.getProperties)
    .map(_.asScala.mapValues(SafeSchema.apply).toMap)
    .getOrElse(Map.empty)
  def requiredFields: List[String] =
    Option(s.getRequired).map(_.asScala.toList).getOrElse(List.empty)
}
sealed abstract class SafePrimitiveSchema(s: Schema[_]) extends SafeSchema(s)
class SafeArraySchema(s: ArraySchema) extends SafeSchema(s) {
  def items: SafeSchema = SafeSchema(s.getItems)
  override def isArray = true
}
class SafeBinarySchema(s: BinarySchema) extends SafeSchema(s)
class SafeBooleanSchema(s: BooleanSchema) extends SafeSchema(s) {
  def default: Option[Boolean] = Option(s.getDefault).map(_.booleanValue())
}
class SafeByteArraySchema(s: ByteArraySchema) extends SafeSchema(s)
class SafeDateSchema(s: DateSchema) extends SafeSchema(s)
class SafeDateTimeSchema(s: DateTimeSchema) extends SafeSchema(s)
class SafeEmailSchema(s: EmailSchema) extends SafeSchema(s)
class SafeFileSchema(s: FileSchema) extends SafeSchema(s)
class SafeIntegerSchema(s: IntegerSchema) extends SafeSchema(s) {
  def default: Option[Int] = Option(s.getDefault).map(_.intValue())
}
class SafeLongSchema(s: IntegerSchema) extends SafeSchema(s) {
  def default: Option[Long] = Option(s.getDefault).map(_.longValue())
}
class SafeMapSchema(s: MapSchema) extends SchemaWithProperties(s) {
  def additionalProperties: Either[Boolean, SafeSchema] =
    Option(s.getAdditionalProperties)
      .map {
        case v: SafeSchema => Right(v)
        case v             => Left(v.asInstanceOf[Boolean])
      }
      .getOrElse(Left(false))
}
class SafeDoubleSchema(s: NumberSchema) extends SafeSchema(s) {
  def default: Option[Double] = Option(s.getDefault).map(_.doubleValue())
}
class SafeFloatSchema(s: NumberSchema) extends SafeSchema(s) {
  def default: Option[Float] = Option(s.getDefault).map(_.floatValue())
}
class SafeObjectSchema(s: ObjectSchema) extends SchemaWithProperties(s)
class SafePasswordSchema(s: PasswordSchema) extends SafeSchema(s)
class SafeStringSchema(s: StringSchema) extends SafeSchema(s) {
  def default: Option[String] = Option(s.getDefault)
}
class SafeUUIDSchema(s: UUIDSchema) extends SafeSchema(s)
class SafeRefSchema(s: Schema[_]) extends SafeSchema(s) {
  def ref: SchemaRef = SchemaRef.parse(s.get$ref)
}
class SafeComposedSchema(s: ComposedSchema) extends SafeSchema(s) {
  def oneOf: List[SafeRefSchema] =
    s.getOneOf.asScala.map(new SafeRefSchema(_)).toList

  def discriminator: Option[SafeDiscriminator] =
    Option(s.getDiscriminator).map(new SafeDiscriminator(_))
  override def toString: String = s.toString
}

class SafeDiscriminator(d: Discriminator) {
  def propertyName: String = d.getPropertyName
  def mapping: Map[String, SchemaRef] = Option(d.getMapping)
    .map(_.asScala.mapValues(SchemaRef.parse).toMap)
    .getOrElse(Map.empty)
}

object SafeSchema {
  def apply(s: Schema[_]): SafeSchema =
    s match {
      case as: ArraySchema      => new SafeArraySchema(as)
      case bs: BooleanSchema    => new SafeBooleanSchema(bs)
      case bas: ByteArraySchema => new SafeByteArraySchema(bas)
      case ds: DateSchema       => new SafeDateSchema(ds)
      case dts: DateTimeSchema  => new SafeDateTimeSchema(dts)
      case es: EmailSchema      => new SafeEmailSchema(es)
      case fs: FileSchema       => new SafeFileSchema(fs)
      case is: IntegerSchema if Option(is.getFormat).contains("int64") =>
        new SafeLongSchema(is)
      case is: IntegerSchema => new SafeIntegerSchema(is)
      case ms: MapSchema     => new SafeMapSchema(ms)
      case ns: NumberSchema if Option(ns.getFormat).contains("float") =>
        new SafeFloatSchema(ns)
      case ns: NumberSchema               => new SafeDoubleSchema(ns)
      case os: ObjectSchema               => new SafeObjectSchema(os)
      case ps: PasswordSchema             => new SafePasswordSchema(ps)
      case ss: StringSchema               => new SafeStringSchema(ss)
      case us: UUIDSchema                 => new SafeUUIDSchema(us)
      case other if other.get$ref != null => new SafeRefSchema(other)
      case composed: ComposedSchema if composed.getOneOf != null =>
        new SafeComposedSchema(composed)
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
      throw new IllegalArgumentException(ref)
    }
}
case class StatusCodeResponse(schema: SafeSchema, statusCode: Int)
