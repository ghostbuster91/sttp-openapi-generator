package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit val dogDecoder: Decoder[Dog] =
    Decoder.forProduct3("className", "color", "breed")(Dog.apply)
  implicit val dogEncoder: Encoder[Dog] =
    Encoder.forProduct3("className", "color", "breed")(p =>
      (p.className, p.color, p.breed)
    )
  implicit val catDecoder: Decoder[Cat] =
    Decoder.forProduct3("className", "color", "breed")(Cat.apply)
  implicit val catEncoder: Encoder[Cat] =
    Encoder.forProduct3("className", "color", "breed")(p =>
      (p.className, p.color, p.breed)
    )
  implicit val animalDecoder: Decoder[Animal] = List[Decoder[Animal]](
    Decoder[Dog].asInstanceOf[Decoder[Animal]],
    Decoder[Cat].asInstanceOf[Decoder[Animal]]
  ).reduceLeft(_ or _)
  implicit val animalEncoder: Encoder[Animal] = Encoder.instance({
    case dog: Dog => Encoder[Dog].apply(dog)
    case cat: Cat => Encoder[Cat].apply(cat)
  })
  implicit val breedAbleDecoder: Decoder[BreedAble] = List[Decoder[BreedAble]](
    Decoder[Dog].asInstanceOf[Decoder[BreedAble]],
    Decoder[Cat].asInstanceOf[Decoder[BreedAble]]
  ).reduceLeft(_ or _)
  implicit val breedAbleEncoder: Encoder[BreedAble] = Encoder.instance({
    case dog: Dog => Encoder[Dog].apply(dog)
    case cat: Cat => Encoder[Cat].apply(cat)
  })
}
object CirceCodecs extends CirceCodecs

sealed trait Animal {
  def className: String
  def color: Option[String]
}

sealed trait BreedAble {
  def breed: Option[String]
}

case class Cat(
    className: String,
    color: Option[String] = Some("red"),
    breed: Option[String]
) extends Animal()
    with BreedAble()

case class Dog(
    className: String,
    color: Option[String] = Some("red"),
    breed: Option[String]
) extends Animal()
    with BreedAble()

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
