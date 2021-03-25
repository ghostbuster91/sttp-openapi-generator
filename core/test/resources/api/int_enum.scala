package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._
import _root_.java.io.File

sealed trait PersonStatus
object PersonStatus {
  case object `1` extends PersonStatus
  case object `2` extends PersonStatus
  case object `3` extends PersonStatus
}

case class Person(status: PersonStatus)

class Api(baseUrl: String) {
  def getPerson(): Request[Person, Any] =
    basicRequest.get(uri"$baseUrl/person").response(asJson[Person].getRight)
}
