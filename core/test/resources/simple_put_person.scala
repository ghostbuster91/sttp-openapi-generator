package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

case class Person(name: String, age: Int)

class Api(serverUrl: String) {
  def updatePerson(aPerson: Person) = basicRequest
    .put(Uri.unsafeApply("https", serverUrl, Seq.empty))
    .body(aPerson)
    .response(asJson[Person])
}
