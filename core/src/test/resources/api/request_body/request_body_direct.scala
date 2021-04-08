package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

case class PetResponse(name: String)
case class PetRequest(id: String)

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def addPet(petRequest: PetRequest): Request[PetResponse, Any] = basicRequest
    .post(uri"$baseUrl/pet")
    .body(petRequest)
    .response(asJson[PetResponse].getRight)
}
