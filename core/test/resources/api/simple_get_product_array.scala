package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

import _root_.java.io.File

case class Category(
    id: Int,
    name: String
)

case class Pet(
    id: Int,
    name: String,
    categories: List[Category],
    status: String
)

class Api(baseUrl: String) {
  def getRoot(): Request[Pet, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(asJson[Pet].getRight)
}
