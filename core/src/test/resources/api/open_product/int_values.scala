package io.github.ghostbuster91.sttp.client3.example
import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.HCursor
import _root_.io.circe.Decoder.Result
import _root_.io.circe.Encoder
import _root_.io.circe.Json
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi {
  implicit val personDecoder: Decoder[Person] = new Decoder[Person]() {
    override def apply(c: HCursor): Result[Person] =
      for (
        name <- c.downField("name").as[String];
        age <- c.downField("age").as[Int];
        additionalProperties <- c.as[Map[String, Int]]
      )
        yield Person(
          name,
          age,
          additionalProperties
            .filterKeys(_ != "name")
            .filterKeys(_ != "age")
        )
  }
  implicit val personEncoder: Encoder[Person] = new Encoder[Person]() {
    override def apply(person: Person): Json =
      Encoder
        .forProduct2[Person, String, Int]("name", "age")(p => (p.name, p.age))
        .apply(person)
        .deepMerge(
          Encoder[Map[String, Int]].apply(person._additionalProperties)
        )
  }

}

case class Person(
    name: String,
    age: Int,
    _additionalProperties: Map[String, Int]
)

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(): Request[Person, Any] =
    basicRequest
      .get(uri"$baseUrl")
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
