package io.github.ghostbuster91.sttp.client3.openapi.zz

case class OpenapiComponent(schemas: Map[String, OpenapiSchemaType])

object OpenapiComponent {
  import io.circe._

  implicit val OpenapiComponentDecoder: Decoder[OpenapiComponent] = {
    (c: HCursor) =>
      for {
        schemas <- c
          .downField("schemas")
          .as[Option[Map[String, OpenapiSchemaType]]]
      } yield OpenapiComponent(schemas.getOrElse(Map.empty))
  }
}
