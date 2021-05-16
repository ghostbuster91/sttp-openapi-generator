package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi
trait CirceCodecs extends SttpCirceApi {
  implicit val personDecoder: Decoder[Person] =
    Decoder.forProduct2("name", "age")(Person.apply)
  implicit val personEncoder: Encoder[Person] =
    Encoder.forProduct2("name", "age")(p => (p.name, p.age))
  implicit val bodyDecoder: Decoder[Body] =
    Decoder.forProduct2("name", "age")(Body.apply)
  implicit val bodyEncoder: Encoder[Body] =
    Encoder.forProduct2("name", "age")(p => (p.name, p.age))
}
object CirceCodecs extends CirceCodecs

case class Body(name: String, age: Option[Int])
case class Person(name: String, age: Int)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def updatePerson(body: Body): Request[Person, Any] = basicRequest
    .put(uri"$baseUrl/person")
    .body(
      List(
        Some("name" -> body.name),
        body.age.map(age => "age" -> age.toString)
      ).flatten.toMap[String, String]
    )
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
