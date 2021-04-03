package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi {
  implicit val personStatusDecoder: Decoder[PersonStatus] =
    Decoder.decodeInt.emap({
      case 1 =>
        Right(PersonStatus.`1`)
      case 2 =>
        Right(PersonStatus.`2`)
      case 3 =>
        Right(PersonStatus.`3`)
      case other =>
        Left("Unexpected value for enum:" + other)
    })
  implicit val personStatusEncoder: Encoder[PersonStatus] =
    Encoder.encodeInt.contramap({
      case PersonStatus.`1` => 1
      case PersonStatus.`2` => 2
      case PersonStatus.`3` => 3
    })
}

sealed trait PersonStatus
object PersonStatus {
  case object `1` extends PersonStatus()
  case object `2` extends PersonStatus()
  case object `3` extends PersonStatus()
}

case class Person(status: PersonStatus)

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getPerson(): Request[Person, Any] =
    basicRequest.get(uri"$baseUrl/person").response(asJson[Person].getRight)
}
