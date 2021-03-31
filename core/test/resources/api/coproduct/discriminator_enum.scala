package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._
import _root_.java.io.File

sealed trait PersonName
object PersonName {
  case object Bob extends PersonName()
  case object Alice extends PersonName()
}

sealed trait Entity {
  def name: PersonName
}
case class Person(name: PersonName, age: Int) extends Entity()
case class Organization(name: PersonName) extends Entity()

class DefaultApi(baseUrl: String) {
  def getRoot(): Request[Entity, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(asJson[Entity].getRight)
}
