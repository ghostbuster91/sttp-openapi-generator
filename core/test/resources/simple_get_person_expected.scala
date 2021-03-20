package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

case class Person(name: String, age: Int)

class Api(serverUrl: String) {
  val getPerson = basicRequest
    .get(Uri.unsafeApply("https", serverUrl, Seq.empty))
    .response(asJson[Person])
}
