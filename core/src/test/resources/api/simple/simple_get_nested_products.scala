package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit val categoryDecoder: Decoder[Category] =
    Decoder.forProduct2("id", "name")(Category.apply)
  implicit val categoryEncoder: Encoder[Category] =
    Encoder.forProduct2("id", "name")(p => (p.id, p.name))
  implicit val petDecoder: Decoder[Pet] =
    Decoder.forProduct4("id", "name", "category", "status")(Pet.apply)
  implicit val petEncoder: Encoder[Pet] =
    Encoder.forProduct4("id", "name", "category", "status")(p =>
      (p.id, p.name, p.category, p.status)
    )
}
object CirceCodecs extends CirceCodecs

case class Category(id: Int, name: String)

case class Pet(id: Int, name: String, category: Category, status: String)

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
