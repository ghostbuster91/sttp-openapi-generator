package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

sealed trait Animal {
  def className: String
  def color: Option[String]
}

case class Dog(
    className: String,
    color: Option[String] = Some("red"),
    breed: Option[String]
) extends Animal()

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(): Request[Dog, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(
      fromMetadata(
        asJson[Dog].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Dog].getRight
        )
      )
    )
}
