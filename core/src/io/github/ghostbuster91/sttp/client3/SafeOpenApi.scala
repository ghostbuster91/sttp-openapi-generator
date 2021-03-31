package io.github.ghostbuster91.sttp.client3

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.Components
import scala.collection.JavaConverters._
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.PathItem
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.parameters.PathParameter
import io.swagger.v3.oas.models.parameters.QueryParameter
import io.swagger.v3.oas.models.parameters.CookieParameter
import io.swagger.v3.oas.models.parameters.HeaderParameter
import io.swagger.v3.oas.models.parameters.RequestBody
import io.swagger.v3.oas.models.media.MediaType
import io.swagger.v3.oas.models.media.ObjectSchema
import io.swagger.v3.oas.models.media.ArraySchema
import io.swagger.v3.oas.models.media._

class SafeOpenApi(openApi: OpenAPI) {
  def components: Option[SafeComponents] =
    Option(openApi.getComponents()).map(new SafeComponents(_))
  def paths: Map[String, SafePathItem] =
    openApi
      .getPaths()
      .asScala
      .toMap
      .mapValues(item => new SafePathItem(item))
      .toMap

  override def toString(): String = openApi.toString()
}

class SafeComponents(c: Components) {
  def schemas: Map[String, SafeSchema] =
    Option(c.getSchemas())
      .map(_.asScala.mapValues(SafeSchema.apply).toMap)
      .getOrElse(Map.empty)
  def requestBodies: Map[String, SafeRequestBody] =
    Option(c.getRequestBodies())
      .map(_.asScala.mapValues(new SafeRequestBody(_)).toMap)
      .getOrElse(Map.empty)
  override def toString(): String = c.toString()
}

class SafeRequestBody(rb: RequestBody) {
  def content: Map[String, SafeMediaType] =
    Option(rb.getContent())
      .map(_.asScala.mapValues(new SafeMediaType(_)).toMap)
      .getOrElse(Map.empty)

  def required: Boolean = rb.getRequired()
  override def toString(): String = rb.toString()
}

class SafePathItem(p: PathItem) {
  def operations: Map[Method, SafeOperation] =
    List(
      Option(p.getGet).map(op => (Method.Get: Method) -> new SafeOperation(op)),
      Option(p.getPut()).map(op =>
        (Method.Put: Method) -> new SafeOperation(op),
      ),
      Option(p.getPost()).map(op =>
        (Method.Post: Method) -> new SafeOperation(op),
      ),
    ).flatten.toMap
  override def toString(): String = p.toString()
}

class SafeOperation(op: Operation) {
  def operationId: String = op.getOperationId()
  def tags: Option[List[String]] =
    Option(op.getTags()).map(_.asScala.toList)

  def parameters: List[SafeParameter] =
    Option(op.getParameters())
      .map(_.asScala.toList.map { p =>
        p match {
          case pp: PathParameter   => new SafePathParameter(pp)
          case cp: CookieParameter => new SafeCookieParameter(cp)
          case hp: HeaderParameter => new SafeHeaderParameter(hp)
          case qp: QueryParameter  => new SafeQueryParameter(qp)
        }
      })
      .getOrElse(List.empty)

  def responses: Map[String, SafeApiResponse] =
    Option(op.getResponses())
      .map(_.asScala.mapValues(r => new SafeApiResponse(r)).toMap)
      .getOrElse(Map.empty)

  def requestBody: Option[SafeRequestBody] =
    Option(op.getRequestBody()).map(new SafeRequestBody(_))

  override def toString(): String = op.toString()
}

sealed abstract class SafeParameter(p: Parameter) {
  def name: String = p.getName()
  def schema: SafeSchema = SafeSchema(p.getSchema())
  def required: Boolean = p.getRequired()
  override def toString(): String = p.toString()

}
class SafePathParameter(p: PathParameter) extends SafeParameter(p)
class SafeHeaderParameter(p: HeaderParameter) extends SafeParameter(p)
class SafeCookieParameter(p: CookieParameter) extends SafeParameter(p)
class SafeQueryParameter(p: QueryParameter) extends SafeParameter(p)
class SafeApiResponse(r: ApiResponse) {
  def content: Map[String, SafeMediaType] =
    Option(r.getContent())
      .map(_.asScala.mapValues(v => new SafeMediaType(v)).toMap)
      .getOrElse(Map.empty)
}

class SafeMediaType(m: MediaType) {
  def schema: SafeSchema = SafeSchema(m.getSchema())
}

sealed abstract class SafeSchema(s: Schema[_]) {
  def enum: List[Any] =
    Option(s.getEnum()).map(_.asScala.toList).getOrElse(List.empty)
  def isEnum = enum.nonEmpty
  override def toString(): String = s.toString()
}
sealed abstract class SchemaWithProperties(s: Schema[_]) extends SafeSchema(s) {
  def properties: Map[String, SafeSchema] = Option(s.getProperties)
    .map(_.asScala.mapValues(SafeSchema.apply).toMap)
    .getOrElse(Map.empty)
  def requiredFields: List[String] =
    Option(s.getRequired()).map(_.asScala.toList).getOrElse(List.empty)
}
sealed abstract class SafePrimitiveSchema(s: Schema[_]) extends SafeSchema(s)
class SafeArraySchema(s: ArraySchema) extends SafeSchema(s) {
  def items: SafeSchema = SafeSchema(s.getItems())
}
class SafeBinarySchema(s: BinarySchema) extends SafeSchema(s)
class SafeBooleanSchema(s: BooleanSchema) extends SafeSchema(s)
class SafeByteArraySchema(s: ByteArraySchema) extends SafeSchema(s)
class SafeDateSchema(s: DateSchema) extends SafeSchema(s)
class SafeDateTimeSchema(s: DateTimeSchema) extends SafeSchema(s)
class SafeEmailSchema(s: EmailSchema) extends SafeSchema(s)
class SafeFileSchema(s: FileSchema) extends SafeSchema(s)
class SafeIntegerSchema(s: IntegerSchema) extends SafeSchema(s) {
  def format: Option[String] = Option(s.getFormat)
}
class SafeMapSchema(s: MapSchema) extends SchemaWithProperties(s)
class SafeNumberSchema(s: NumberSchema) extends SafeSchema(s) {
  def format: Option[String] = Option(s.getFormat)
}
class SafeObjectSchema(s: ObjectSchema) extends SchemaWithProperties(s)
class SafePasswordSchema(s: PasswordSchema) extends SafeSchema(s)
class SafeStringSchema(s: StringSchema) extends SafeSchema(s)
class SafeUUIDSchema(s: UUIDSchema) extends SafeSchema(s)
class SafeRefSchema(s: Schema[_]) extends SafeSchema(s) {
  def ref: SchemaRef = SchemaRef.parse(s.get$ref)
}
class SafeComposedSchema(s: ComposedSchema) extends SafeSchema(s) {
  def oneOf: List[SafeRefSchema] =
    s.getOneOf.asScala.map(new SafeRefSchema(_)).toList

  def discriminator: Option[SafeDiscriminator] =
    Option(s.getDiscriminator()).map(new SafeDiscriminator(_))
  override def toString(): String = s.toString()
}

class SafeDiscriminator(d: Discriminator) {
  def propertyName: String = d.getPropertyName()
}

object SafeSchema {
  def apply(s: Schema[_]): SafeSchema =
    s match {
      case as: ArraySchema                => new SafeArraySchema(as)
      case bs: BooleanSchema              => new SafeBooleanSchema(bs)
      case bas: ByteArraySchema           => new SafeByteArraySchema(bas)
      case ds: DateSchema                 => new SafeDateSchema(ds)
      case dts: DateTimeSchema            => new SafeDateTimeSchema(dts)
      case es: EmailSchema                => new SafeEmailSchema(es)
      case fs: FileSchema                 => new SafeFileSchema(fs)
      case is: IntegerSchema              => new SafeIntegerSchema(is)
      case ms: MapSchema                  => new SafeMapSchema(ms)
      case ns: NumberSchema               => new SafeNumberSchema(ns)
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

  def parse(ref: String) =
    if (ref.contains("#/components/schemas/")) {
      SchemaRef.Schema(ref.replaceAll("#/components/schemas/", ""))
    } else if (ref.contains("#/components/requestBodies/")) {
      SchemaRef.RequestBody(ref.replaceAll("#/components/requestBodies/", ""))
    } else {
      throw new IllegalArgumentException(ref)
    }
}
