package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

case class Category(id: Int, name: String)

case class Pet(id: Int, name: String, category: Category, status: String)

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(): Request[Pet, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(
      fromMetadata(
        asJson[Pet].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Pet].getRight
        )
      )
    )
}
