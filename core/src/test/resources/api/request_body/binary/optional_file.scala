package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.java.io.File
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

class PetApi(baseUrl: String) extends CirceCodecs {
  def uploadFile(
      petId: Long,
      additionalMetadata: Option[String],
      file: Option[File]
  ): Request[Unit, Any] = file
    .foldLeft(
      basicRequest.post(
        uri"$baseUrl/pet/$petId/uploadImage?additionalMetadata=$additionalMetadata"
      )
    )((acc, item) => acc.body(item))
    .response(fromMetadata(asJson[Unit].getRight))
}
