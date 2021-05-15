package io.github.ghostbuster91.sttp.client3.example
import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi {
  implicit val statusDecoder: Decoder[Status] = Decoder.decodeString.emap({
    case "happy" =>
      Right(Status.Happy)
    case "neutral" =>
      Right(Status.Neutral)
    case other =>
      Left("Unexpected value for enum:" + other)
  })
  implicit val statusEncoder: Encoder[Status] = Encoder.encodeString.contramap({
    case Status.Happy   => "happy"
    case Status.Neutral => "neutral"
  })
}
object CirceCodecs extends CirceCodecs
sealed trait Status
object Status {
  case object Happy extends Status()
  case object Neutral extends Status()
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
