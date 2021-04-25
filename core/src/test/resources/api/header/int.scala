package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(key: Int): Request[Unit, Any] = basicRequest
    .get(uri"$baseUrl")
    .header("key", key.toString())
    .response(
      fromMetadata(asJson[Unit].getRight)
    )
}
