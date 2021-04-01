package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

class DefaultApi(baseUrl: String) {
  def getRoot(key: String): Request[Unit, Any] = basicRequest
    .get(uri"$baseUrl")
    .header("key", key)
    .response(asJson[Unit].getRight)
}
