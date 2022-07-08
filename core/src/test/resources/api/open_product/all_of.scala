package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Json
import _root_.io.circe.Decoder
import _root_.io.circe.HCursor
import _root_.io.circe.Decoder.Result
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit lazy val dogDecoder: Decoder[Dog] = new Decoder[Dog]() {
    override def apply(c: HCursor): Result[Dog] = for (
      className <- c.downField("className").as[String];
      color <- c.downField("color").as[Option[String]];
      breed <- c.downField("breed").as[Option[String]];
      additionalProperties <- c.as[Map[String, Json]]
    )
      yield Dog(
        className,
        color,
        breed,
        additionalProperties
          .filterKeys(_ != "className")
          .filterKeys(_ != "color")
          .filterKeys(_ != "breed")
      )
  }
  implicit lazy val dogEncoder: Encoder[Dog] = new Encoder[Dog]() {
    override def apply(dog: Dog): Json = Encoder
      .forProduct3[Dog, String, Option[String], Option[String]](
        "className",
        "color",
        "breed"
      )(p => (p.className, p.color, p.breed))
      .apply(dog)
      .deepMerge(Encoder[Map[String, Json]].apply(dog._additionalProperties))
  }
  implicit lazy val animalDecoder: Decoder[Animal] = List[Decoder[Animal]](
    Decoder[Dog].asInstanceOf[Decoder[Animal]]
  ).reduceLeft(_ or _)
  implicit lazy val animalEncoder: Encoder[Animal] = Encoder.instance {
    case dog: Dog =>
      Encoder[Dog].apply(dog)
  }
}
object CirceCodecs extends CirceCodecs

sealed trait Animal {
  def className: String
  def color: Option[String]
  def _additionalProperties: Map[String, Json]
}

case class Dog(
    className: String,
    color: Option[String] = Some("red"),
    breed: Option[String],
    _additionalProperties: Map[String, Json]
) extends Animal()

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(): Request[Dog, Any] = basicRequest
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
}
