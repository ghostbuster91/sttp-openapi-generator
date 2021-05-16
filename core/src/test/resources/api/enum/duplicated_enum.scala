package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit val status2Decoder: Decoder[Status2] = Decoder.decodeString.emap({
    case "new" =>
      Right(Status2.New)
    case "old" =>
      Right(Status2.Old)
    case other =>
      Left("Unexpected value for enum:" + other)
  })
  implicit val status2Encoder: Encoder[Status2] =
    Encoder.encodeString.contramap({
      case Status2.New => "new"
      case Status2.Old => "old"
    })
  implicit val statusDecoder: Decoder[Status] = Decoder.decodeString.emap({
    case "happy" =>
      Right(Status.Happy)
    case "neutral" =>
      Right(Status.Neutral)
    case other =>
      Left("Unexpected value for enum:" + other)
  })
  implicit val statusEncoder: Encoder[Status] =
    Encoder.encodeString.contramap({
      case Status.Happy   => "happy"
      case Status.Neutral => "neutral"
    })
  implicit val person1Decoder: Decoder[Person1] =
    Decoder.forProduct1("status")(Person1.apply)
  implicit val person1Encoder: Encoder[Person1] =
    Encoder.forProduct1("status")(p => p.status)
  implicit val person2Decoder: Decoder[Person2] =
    Decoder.forProduct1("status")(Person2.apply)
  implicit val person2Encoder: Encoder[Person2] =
    Encoder.forProduct1("status")(p => p.status)
  implicit val coupleDecoder: Decoder[Couple] =
    Decoder.forProduct2("p1", "p2")(Couple.apply)
  implicit val coupleEncoder: Encoder[Couple] =
    Encoder.forProduct2("p1", "p2")(p => (p.p1, p.p2))
}
object CirceCodecs extends CirceCodecs

sealed trait Status
object Status {
  case object Happy extends Status()
  case object Neutral extends Status()
}
sealed trait Status2
object Status2 {
  case object New extends Status2()
  case object Old extends Status2()
}
case class Couple(p1: Person1, p2: Person2)
case class Person1(status: Status)
case class Person2(status: Status2)

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
