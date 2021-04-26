package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.{Error => CirceError}
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

case class ErrorModel(msg: String)

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def updatePerson(): Request[
    Either[ResponseException[ErrorModel, CirceError], Unit],
    Any
  ] = basicRequest
    .put(uri"$baseUrl/person")
    .response(
      fromMetadata(
        asJsonEither[ErrorModel, Unit],
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(400),
          asJsonEither[ErrorModel, Unit]
        )
      )
    )
}
