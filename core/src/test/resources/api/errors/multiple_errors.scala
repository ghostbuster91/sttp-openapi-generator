package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.{Error => CirceError}
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit val errorModelDecoder: Decoder[ErrorModel] =
    Decoder.forProduct1("msg")(ErrorModel.apply)
  implicit val errorModelEncoder: Encoder[ErrorModel] =
    Encoder.forProduct1("msg")(p => p.msg)
  implicit val errorModel2Decoder: Decoder[ErrorModel2] =
    Decoder.forProduct1("msg")(ErrorModel2.apply)
  implicit val errorModel2Encoder: Encoder[ErrorModel2] =
    Encoder.forProduct1("msg")(p => p.msg)
  implicit val updatePersonGenericErrorDecoder
      : Decoder[UpdatePersonGenericError] =
    List[Decoder[UpdatePersonGenericError]](
      Decoder[ErrorModel].asInstanceOf[Decoder[UpdatePersonGenericError]],
      Decoder[ErrorModel2].asInstanceOf[Decoder[UpdatePersonGenericError]]
    ).reduceLeft(_ or _)
  implicit val updatePersonGenericErrorEncoder
      : Encoder[UpdatePersonGenericError] = Encoder.instance {
    case errorModel: ErrorModel =>
      Encoder[ErrorModel].apply(errorModel)
    case errorModel2: ErrorModel2 =>
      Encoder[ErrorModel2].apply(errorModel2)
  }
}
object CirceCodecs extends CirceCodecs

sealed trait UpdatePersonGenericError
case class ErrorModel(msg: String) extends UpdatePersonGenericError()
case class ErrorModel2(msg: String) extends UpdatePersonGenericError()

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
          asJsonEither[ErrorModel2, Unit]
        )
      )
    )
}
