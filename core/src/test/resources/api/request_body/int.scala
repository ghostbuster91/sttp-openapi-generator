package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def updatePerson(int: Int): Request[Int, Any] = basicRequest
    .put(uri"$baseUrl/person")
    .body(int)
    .response(
      fromMetadata(
        asJson[Int].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Int].getRight
        )
      )
    )
}
