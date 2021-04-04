package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

sealed trait Entity {
  def name: Int
}
case class Person(name: Int, age: Int) extends Entity()
case class Organization(name: Int) extends Entity()

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(): Request[Entity, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(asJson[Entity].getRight)
}
