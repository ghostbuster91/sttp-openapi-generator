package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi
object CirceCodecs extends CirceCodecs

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def updatePerson(boolean: Boolean = true): Request[Boolean, Any] =
    basicRequest
      .put(uri"$baseUrl/person")
      .body(boolean)
      .response(
        fromMetadata(
          asJson[Boolean].getRight,
          ConditionalResponseAs(
            _.code == StatusCode.unsafeApply(200),
            asJson[Boolean].getRight
          )
        )
      )
}
