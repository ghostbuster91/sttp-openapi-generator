package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi
trait CirceCodecs extends SttpCirceApi {

  implicit lazy val petRequestDecoder: Decoder[PetRequest] =
    Decoder.forProduct1("id")(PetRequest.apply)
  implicit lazy val petRequestEncoder: Encoder[PetRequest] =
    Encoder.forProduct1("id")(p => p.id)
  implicit lazy val petResponseDecoder: Decoder[PetResponse] =
    Decoder.forProduct1("name")(PetResponse.apply)
  implicit lazy val petResponseEncoder: Encoder[PetResponse] =
    Encoder.forProduct1("name")(p => p.name)
}
object CirceCodecs extends CirceCodecs

case class PetRequest(id: String)
case class PetResponse(name: String)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def addPet(petRequest: PetRequest): Request[PetResponse, Any] = basicRequest
    .post(uri"$baseUrl/pet")
    .body(petRequest)
    .response(
      fromMetadata(
        asJson[PetResponse].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[PetResponse].getRight
        )
      )
    )
}
