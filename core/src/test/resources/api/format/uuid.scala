package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.java.util.UUID
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi
object CirceCodecs extends CirceCodecs

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(): Request[UUID, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(
      fromMetadata(
        asJson[UUID].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[UUID].getRight
        )
      )
    )
}
