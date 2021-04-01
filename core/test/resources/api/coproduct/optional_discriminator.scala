package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

sealed trait Entity {
  def name: Option[String]
}
case class Person(name: Option[String], age: Int) extends Entity()
case class Organization(name: Option[String]) extends Entity()

class DefaultApi(baseUrl: String) {
  def getRoot(): Request[Entity, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(asJson[Entity].getRight)
}
