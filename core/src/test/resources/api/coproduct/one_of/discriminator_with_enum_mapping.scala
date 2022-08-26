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
  implicit lazy val personNameDecoder: Decoder[PersonName] =
    Decoder.decodeString.emap {
      case "alice" =>
        Right(PersonName.Alice)
      case "bob" =>
        Right(PersonName.Bob)
      case other =>
        Left("Unexpected value for enum:" + other)
    }
  implicit lazy val personNameEncoder: Encoder[PersonName] =
    Encoder.encodeString.contramap {
      case PersonName.Alice => "alice"
      case PersonName.Bob   => "bob"
    }
  implicit lazy val organizationDecoder: Decoder[Organization] =
    Decoder.const(Organization.apply)
  implicit lazy val organizationEncoder: Encoder[Organization] =
    Encoder.forProduct1("name")(_ => "alice")
  implicit lazy val personDecoder: Decoder[Person] =
    Decoder.forProduct1("age")(Person.apply)
  implicit lazy val personEncoder: Encoder[Person] =
    Encoder.forProduct2("name", "age")(p => ("bob", p.age))
  implicit lazy val entityDecoder: Decoder[Entity] = new Decoder[Entity]() {
    override def apply(c: HCursor): Result[Entity] = c.downField("name").as[PersonName].flatMap({
      case PersonName.Alice =>
        Decoder[Organization].apply(c)
      case PersonName.Bob =>
        Decoder[Person].apply(c)
      case other =>
        Left(DecodingFailure("Unexpected value for coproduct:" + other, Nil))
    })
  }
  implicit lazy val entityEncoder: Encoder[Entity] = Encoder.instance{
    case organization: Organization =>
      Encoder[Organization].apply(organization)
    case person: Person =>
      Encoder[Person].apply(person)
  }
}
object CirceCodecs extends CirceCodecs

sealed trait PersonName
object PersonName {
  case object Bob extends PersonName()
  case object Alice extends PersonName()
}

sealed trait Entity
case class Organization() extends Entity()
case class Person(age: Int) extends Entity()

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
