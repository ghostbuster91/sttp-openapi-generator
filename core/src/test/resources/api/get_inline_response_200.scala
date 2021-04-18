package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

case class InlineResponse200(status: Int)

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getPersonById(): Request[InlineResponse200, Any] = basicRequest
    .get(uri"$baseUrl/person")
    .response(
      fromMetadata(
        asJson[InlineResponse200].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[InlineResponse200].getRight
        )
      )
    )
}
