package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit val organizationDecoder: Decoder[Organization] =
    Decoder.forProduct1("name")(Organization.apply)
  implicit val organizationEncoder: Encoder[Organization] =
    Encoder.forProduct1("name")(p => p.name)
  implicit val personDecoder: Decoder[Person] =
    Decoder.forProduct2("name", "age")(Person.apply)
  implicit val personEncoder: Encoder[Person] =
    Encoder.forProduct2("name", "age")(p => (p.name, p.age))
  implicit val doubledEntityDecoder: Decoder[DoubledEntity] =
    List[Decoder[DoubledEntity]](
      Decoder[Organization].asInstanceOf[Decoder[DoubledEntity]],
      Decoder[Person].asInstanceOf[Decoder[DoubledEntity]]
    ).reduceLeft(_ or _)
  implicit val doubledEntityEncoder: Encoder[DoubledEntity] = Encoder.instance {
    case organization: Organization => Encoder[Organization].apply(organization)
    case person: Person             => Encoder[Person].apply(person)
  }
  implicit val entityDecoder: Decoder[Entity] = List[Decoder[Entity]](
    Decoder[Organization].asInstanceOf[Decoder[Entity]],
    Decoder[Person].asInstanceOf[Decoder[Entity]]
  ).reduceLeft(_ or _)
  implicit val entityEncoder: Encoder[Entity] = Encoder.instance {
    case organization: Organization => Encoder[Organization].apply(organization)
    case person: Person             => Encoder[Person].apply(person)
  }
}
object CirceCodecs extends CirceCodecs

sealed trait DoubledEntity
sealed trait Entity

case class Organization(name: String) extends DoubledEntity() with Entity()

case class Person(name: String, age: Int) extends DoubledEntity() with Entity()

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(): Request[Entity, Any] =
    basicRequest
      .get(uri"$baseUrl")
      .response(
        fromMetadata(
          asJson[Entity].getRight,
          ConditionalResponseAs(
            _.code == StatusCode.unsafeApply(200),
            asJson[Entity].getRight
          )
        )
      )
}
