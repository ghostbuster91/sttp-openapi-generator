package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi
object CirceCodecs extends CirceCodecs

case class Person(name: String, age: Int)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def updatePerson(person: Person): Request[Person, Any] = basicRequest
    .put(uri"$baseUrl/person")
    .body(person)
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
