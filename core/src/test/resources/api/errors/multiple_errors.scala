package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.{Error => CirceError}
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

sealed trait UpdatePersonGenericError
case class ErrorModel(msg: String) extends UpdatePersonGenericError()
case class ErrorModel2(msg: String) extends UpdatePersonGenericError()

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def updatePerson(): Request[
    Either[ResponseException[UpdatePersonGenericError, CirceError], Unit],
    Any
  ] = basicRequest
    .put(uri"$baseUrl/person")
    .response(
      fromMetadata(
        asJsonEither[UpdatePersonGenericError, Unit],
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(400),
          asJsonEither[ErrorModel, Unit]
        ),
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(401),
          asJsonEither[ErrorModel2, Unit]
        )
      )
    )
}
