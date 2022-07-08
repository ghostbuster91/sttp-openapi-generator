package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit lazy val categoryDecoder: Decoder[Category] =
    Decoder.forProduct2("id", "name")(Category.apply)
  implicit lazy val categoryEncoder: Encoder[Category] =
    Encoder.forProduct2("id", "name")(p => (p.id, p.name))
  implicit lazy val petDecoder: Decoder[Pet] =
    Decoder.forProduct4("id", "name", "categories", "status")(Pet.apply)
  implicit lazy val petEncoder: Encoder[Pet] =
    Encoder.forProduct4("id", "name", "categories", "status")(p =>
      (p.id, p.name, p.categories, p.status)
    )
}
object CirceCodecs extends CirceCodecs

case class Category(
    id: Int,
    name: String
)

case class Pet(
    id: Int,
    name: String,
    categories: List[Category],
    status: String
)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(): Request[Pet, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(
      fromMetadata(
        asJson[Pet].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Pet].getRight
        )
      )
    )
}
