package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

case class Person(name: String, age: Int)
case class Body(name: String, age: Option[Int])

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def updatePerson(body: Body): Request[Person, Any] = basicRequest
    .put(uri"$baseUrl/person")
    .body(
      List(
        List("name" -> body.name),
        body.age.map(age => "age" -> age.toString)
      ).flatten
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
