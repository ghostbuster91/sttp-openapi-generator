package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi
object CirceCodecs extends CirceCodecs

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getSomething(
      petId: Int,
      additionalMetadata: Option[String]
  ): Request[Unit, Any] = basicRequest
    .get(
      uri"$baseUrl/pet/$petId/uploadImage?additionalMetadata=$additionalMetadata"
    )
    .response(
      fromMetadata(asJson[Unit].getRight)
    )
}
