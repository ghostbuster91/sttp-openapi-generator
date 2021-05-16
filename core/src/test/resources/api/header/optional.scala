package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi
object CirceCodecs extends CirceCodecs

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(key: Option[String]): Request[Unit, Any] = basicRequest
    .get(uri"$baseUrl")
    .headers(key.map(v => Header("key", v.toString())).toList: _*)
    .response(
      fromMetadata(asJson[Unit].getRight)
    )
}
