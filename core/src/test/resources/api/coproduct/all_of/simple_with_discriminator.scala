package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.HCursor
import _root_.io.circe.DecodingFailure
import _root_.io.circe.Decoder.Result
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit lazy val dogDecoder: Decoder[Dog] =
    Decoder.forProduct1("bark")(Dog.apply)
  implicit lazy val dogEncoder: Encoder[Dog] =
    Encoder.forProduct2("petType", "bark")(p => ("dog", p.bark))
  implicit lazy val lizardDecoder: Decoder[Lizard] =
    Decoder.forProduct1("lovesRocks")(Lizard.apply)
  implicit lazy val lizardEncoder: Encoder[Lizard] =
    Encoder.forProduct2("petType", "lovesRocks")(p => ("Lizard", p.lovesRocks))
  implicit lazy val petDecoder: Decoder[Pet] = new Decoder[Pet]() {
    override def apply(c: HCursor): Result[Pet] = c
      .downField("petType")
      .as[String]
      .flatMap {
        case "Lizard" =>
          Decoder[Lizard].apply(c)
        case "dog" =>
          Decoder[Dog].apply(c)
        case other =>
          Left(DecodingFailure("Unexpected value for coproduct:" + other, Nil))
      }
  }
  implicit lazy val petEncoder: Encoder[Pet] = Encoder.instance {
    case dog: Dog =>
      Encoder[Dog].apply(dog)
    case lizard: Lizard =>
      Encoder[Lizard].apply(lizard)
  }
}
object CirceCodecs extends CirceCodecs
sealed trait Pet
case class Dog(bark: Option[String]) extends Pet()
case class Lizard(lovesRocks: Option[Boolean]) extends Pet()
class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._
  def getDog(): Request[Dog, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(
      fromMetadata(
        asJson[Dog].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Dog].getRight
        )
      )
    )
  def getLizard(): Request[Lizard, Any] = basicRequest
    .get(uri"$baseUrl/lizard")
    .response(
      fromMetadata(
        asJson[Lizard].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Lizard].getRight
        )
      )
    )
}
