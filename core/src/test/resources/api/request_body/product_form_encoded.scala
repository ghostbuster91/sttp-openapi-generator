package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.sttp.client3.circe.SttpCirceApi
trait CirceCodecs extends SttpCirceApi {
  implicit lazy val personDecoder: Decoder[Person] =
    Decoder.forProduct2("name", "age")(Person.apply)
  implicit lazy val personEncoder: Encoder[Person] =
    Encoder.forProduct2("name", "age")(p => (p.name, p.age))
  implicit lazy val personBodyDecoder: Decoder[PersonBody] =
    Decoder.forProduct2("name", "age")(PersonBody.apply)
  implicit lazy val personBodyEncoder: Encoder[PersonBody] =
    Encoder.forProduct2("name", "age")(p => (p.name, p.age))
}
object CirceCodecs extends CirceCodecs

case class Person(name: String, age: Int)
case class PersonBody(name: String, age: Option[Int])

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def updatePerson(personBody: PersonBody): Request[Person, Any] = basicRequest
    .put(uri"$baseUrl/person")
    .body(
      List(
        Some("name" -> personBody.name),
        personBody.age.map(age => "age" -> age.toString)
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
