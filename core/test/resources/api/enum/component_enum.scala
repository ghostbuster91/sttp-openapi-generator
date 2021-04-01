package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

sealed trait Status
object Status {
  case object Happy extends Status()
  case object Neutral extends Status()
  case object Sad extends Status()
}

case class Person(status: Status)

class DefaultApi(baseUrl: String) {
  def getPerson(): Request[Person, Any] =
    basicRequest.get(uri"$baseUrl/person").response(asJson[Person].getRight)
}
