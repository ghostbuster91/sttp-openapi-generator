package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {

  implicit lazy val organizationDecoder: Decoder[Organization] =
    Decoder.forProduct1("name")(Organization.apply)
  implicit lazy val organizationEncoder: Encoder[Organization] =
    Encoder.forProduct1("name")(p => p.name)
  implicit lazy val personDecoder: Decoder[Person] =
    Decoder.forProduct2("name", "age")(Person.apply)
  implicit lazy val personEncoder: Encoder[Person] =
    Encoder.forProduct2("name", "age")(p => (p.name, p.age))
  implicit lazy val entityDecoder: Decoder[Entity] = List[Decoder[Entity]](
    Decoder[Organization].asInstanceOf[Decoder[Entity]],
    Decoder[Person].asInstanceOf[Decoder[Entity]]
  ).reduceLeft(_ or _)
  implicit lazy val entityEncoder: Encoder[Entity] = Encoder.instance {
    case organization: Organization =>
      Encoder[Organization].apply(organization)
    case person: Person =>
      Encoder[Person].apply(person)
  }
}
object CirceCodecs extends CirceCodecs

sealed trait Entity {
  def name: String
}
case class Organization(name: String) extends Entity()
case class Person(name: String, age: Int) extends Entity()

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(): Request[Entity, Any] = basicRequest
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
