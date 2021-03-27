package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

import _root_.java.io.File

case class Person(name: String, age: Int)

class Api(baseUrl: String) {
  def getPersonById(personId: Int): Request[Person, Any] = basicRequest
    .get(uri"$baseUrl/person/$personId")
    .response(asJson[Person].getRight)
}