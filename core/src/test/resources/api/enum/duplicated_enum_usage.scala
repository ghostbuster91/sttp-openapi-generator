package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit lazy val statusDecoder: Decoder[Status] = Decoder.decodeString.emap {
    case "happy" =>
      Right(Status.Happy)
    case "neutral" =>
      Right(Status.Neutral)
    case other =>
      Left("Unexpected value for enum:" + other)
  }
  implicit lazy val statusEncoder: Encoder[Status] = Encoder.encodeString.contramap {
    case Status.Happy   => "happy"
    case Status.Neutral => "neutral"
  }
  implicit lazy val coupleDecoder: Decoder[Couple] =
    Decoder.forProduct2("p1", "p2")(Couple.apply)
  implicit lazy val coupleEncoder: Encoder[Couple] =
    Encoder.forProduct2("p1", "p2")(p => (p.p1, p.p2))
  implicit lazy val personDecoder: Decoder[Person] =
    Decoder.forProduct1("status")(Person.apply)
  implicit lazy val personEncoder: Encoder[Person] =
    Encoder.forProduct1("status")(p => p.status)
}
object CirceCodecs extends CirceCodecs

sealed trait Status
object Status {
  case object Happy extends Status()
  case object Neutral extends Status()
}

case class Couple(p1: Person, p2: Person)
case class Person(status: Status)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getPerson(): Request[Couple, Any] =
    basicRequest
      .get(uri"$baseUrl/person")
      .response(
        fromMetadata(
          asJson[Couple].getRight,
          ConditionalResponseAs(
            _.code == StatusCode.unsafeApply(200),
            asJson[Couple].getRight
          )
        )
      )
}
