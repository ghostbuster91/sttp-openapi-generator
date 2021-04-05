package io.github.ghostbuster91.sttp.client3.example
import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getSomething(
      petId: Int,
      additionalMetadata: Option[String],
  ): Request[Unit, Any] = basicRequest
    .get(
      uri"$baseUrl/pet/$petId/uploadImage?additionalMetadata=$additionalMetadata",
    )
    .response(asJson[Unit].getRight)
}