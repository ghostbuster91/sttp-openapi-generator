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
  implicit lazy val updatePersonGenericErrorDecoder
      : Decoder[UpdatePersonGenericError] =
    List[Decoder[UpdatePersonGenericError]](
      Decoder[ErrorModel].asInstanceOf[Decoder[UpdatePersonGenericError]]
    ).reduceLeft(_ or _)
  implicit lazy val updatePersonGenericErrorEncoder
      : Encoder[UpdatePersonGenericError] = Encoder.instance {
    case errorModel: ErrorModel =>
      Encoder[ErrorModel].apply(errorModel)
  }
}
object CirceCodecs extends CirceCodecs

sealed trait UpdatePersonGenericError
case class ErrorModel(msg: String) extends UpdatePersonGenericError()

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

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
          asJsonEither[ErrorModel, Unit]
        )
      )
    )
}
