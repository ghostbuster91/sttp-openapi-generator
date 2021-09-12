package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit val statusDecoder: Decoder[Status] =
    Decoder.decodeInt.emap {
      case 1 =>
        Right(Status.`1`)
      case 2 =>
        Right(Status.`2`)
      case 3 =>
        Right(Status.`3`)
      case other =>
        Left("Unexpected value for enum:" + other)
    }
  implicit val statusEncoder: Encoder[Status] =
    Encoder.encodeInt.contramap {
      case Status.`1` => 1
      case Status.`2` => 2
      case Status.`3` => 3
    }
  implicit val personDecoder: Decoder[Person] =
    Decoder.forProduct1("status")(Person.apply)
  implicit val personEncoder: Encoder[Person] =
    Encoder.forProduct1("status")(p => p.status)
}
object CirceCodecs extends CirceCodecs

sealed trait Status
object Status {
  case object `1` extends Status()
  case object `2` extends Status()
  case object `3` extends Status()
}

case class Person(status: Status)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getPerson(): Request[Person, Any] =
    basicRequest
      .get(uri"$baseUrl/person")
      .response(
        fromMetadata(
          asJson[Person].getRight,
          ConditionalResponseAs(
            _.code == StatusCode.unsafeApply(200),
            asJson[Person].getRight
          )
        )
      )
}
