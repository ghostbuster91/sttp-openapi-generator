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
    categories: List[Category],
    status: String
)

class Api(serverUrl: String) {
  val getRoot = basicRequest
    .get(Uri.unsafeApply("https", serverUrl, Seq.empty))
    .response(asJson[Pet])
}
