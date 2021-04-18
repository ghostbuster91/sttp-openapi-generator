package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.HCursor
import _root_.io.circe.Json
import _root_.io.circe.DecodingFailure
import _root_.io.circe.Decoder.Result
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi {
  implicit val entityDecoder: Decoder[Entity] = new Decoder[Entity] {
    override def apply(c: HCursor): Result[Entity] = c
      .downField("name")
      .as[String]
      .flatMap({
        case "john" => Decoder[Person].apply(c)
        case "sml"  => Decoder[Organization].apply(c)
        case other =>
          Left(DecodingFailure("Unexpected value for coproduct:" + other, Nil))
      })
  }
  implicit val entityEncoder: Encoder[Entity] = new Encoder[Entity] {
    override def apply(entity: Entity): Json = entity match {
      case person: Person => Encoder[Person].apply(person)
      case organization: Organization =>
        Encoder[Organization].apply(organization)
    }
  }
}
sealed trait Entity { def name: String }
case class Person(name: String, age: Int) extends Entity()
case class Organization(name: String) extends Entity()

class DefaultApi(baseUrl: String) extends CirceCodecs {
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
