package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.sttp.client3.circe._
import _root_.io.circe.generic.auto._

case class Category(
    id: Int,
    name: String
)

case class Pet(
    id: Int,
    name: String,
    category: Category,
    status: String
)

class Api(baseUrl: String) {
  val getRoot = basicRequest
    .get(uri"https://$baseUrl/")
    .response(asJson[Pet])
}
