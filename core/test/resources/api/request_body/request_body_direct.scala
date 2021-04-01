package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

case class PetResponse(name: String)
case class PetRequest(id: String)

class DefaultApi(baseUrl: String) {
  def addPet(aPetRequest: PetRequest): Request[PetResponse, Any] = basicRequest
    .post(uri"$baseUrl/pet")
    .body(aPetRequest)
    .response(asJson[PetResponse].getRight)
}
