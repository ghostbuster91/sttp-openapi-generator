package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi {
  implicit val personStatusDecoder: Decoder[PersonStatus] =
    Decoder.decodeString.emap({
      case "happy" =>
        Right(PersonStatus.Happy)
      case "neutral" =>
        Right(PersonStatus.Neutral)
      case other =>
        Left("Unexpected value for enum:" + other)
    })
  implicit val personStatusEncoder: Encoder[PersonStatus] =
    Encoder.encodeString.contramap({
      case PersonStatus.Happy   => "happy"
      case PersonStatus.Neutral => "neutral"
    })
}

sealed trait PersonStatus
object PersonStatus {
  case object Happy extends PersonStatus()
  case object Neutral extends PersonStatus()
}

case class Person(status: PersonStatus)

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getPerson(): Request[Person, Any] =
    basicRequest.get(uri"$baseUrl/person").response(asJson[Person].getRight)
}
