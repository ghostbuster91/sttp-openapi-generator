package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.{Error => CirceError}
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit lazy val errorModelDecoder: Decoder[ErrorModel] =
    Decoder.forProduct1("msg")(ErrorModel.apply)
  implicit lazy val errorModelEncoder: Encoder[ErrorModel] =
    Encoder.forProduct1("msg")(p => p.msg)
}
object CirceCodecs extends CirceCodecs

case class ErrorModel(msg: String)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

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
