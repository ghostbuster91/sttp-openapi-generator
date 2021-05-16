package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi
object CirceCodecs extends CirceCodecs

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(id: String): Request[Unit, Any] = basicRequest
    .get(uri"$baseUrl?id=$id")
    .response(
      fromMetadata(asJson[Unit].getRight)
    )
}
