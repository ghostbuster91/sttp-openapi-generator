package io.github.ghostbuster91.sttp.client3.example
import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._
import _root_.java.io.File

class DefaultApi(baseUrl: String) {
  def getSomething(
      petId: Int,
      additionalMetadata: Option[String],
  ): Request[Unit, Any] = basicRequest
    .get(
      uri"$baseUrl/pet/$petId/uploadImage?additionalMetadata=$additionalMetadata",
    )
    .response(asJson[Unit].getRight)
}
