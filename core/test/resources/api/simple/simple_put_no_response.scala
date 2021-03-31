package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

import _root_.java.io.File

class DefaultApi(baseUrl: String) {
  def updatePerson(): Request[Unit, Any] = basicRequest
    .put(uri"$baseUrl/person")
    .response(asJson[Unit].getRight)
}
