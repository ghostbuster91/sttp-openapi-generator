package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

sealed trait GetRootGenericSuccess

case class Person(name: String, age: Int) extends GetRootGenericSuccess()

case class Organization(name: String) extends GetRootGenericSuccess()

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(): Request[GetRootGenericSuccess, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(
      fromMetadata(
        asJson[GetRootGenericSuccess].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Person].getRight
        ),
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(201),
          asJson[Organization].getRight
        )
      )
    )
}
