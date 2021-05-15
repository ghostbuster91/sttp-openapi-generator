package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi
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
