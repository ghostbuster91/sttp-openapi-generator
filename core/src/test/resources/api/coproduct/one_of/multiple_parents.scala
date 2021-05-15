package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi
object CirceCodecs extends CirceCodecs

sealed trait DoubledEntity
sealed trait Entity

case class Organization(name: String) extends DoubledEntity() with Entity()

case class Person(name: String, age: Int) extends DoubledEntity() with Entity()

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(): Request[Entity, Any] =
    basicRequest
      .get(uri"$baseUrl")
      .response(
        fromMetadata(
          asJson[Entity].getRight,
          ConditionalResponseAs(
            _.code == StatusCode.unsafeApply(200),
            asJson[Entity].getRight
          )
        )
      )
}
