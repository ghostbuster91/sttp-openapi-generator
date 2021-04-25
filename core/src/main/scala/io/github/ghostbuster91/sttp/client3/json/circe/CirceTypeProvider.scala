package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.ImportRegistry._

import scala.meta._

object CirceTypeProvider extends JsonTypeProvider {
  def anyType: IM[Type.Name] =
    registerExternalTpe(q"import _root_.io.circe.Json")
  def errorType: Type =
    t"io.circe.Error"

  val EncoderTpe = registerExternalTpe(q"import _root_.io.circe.Encoder")
  val DecoderTpe = registerExternalTpe(q"import _root_.io.circe.Decoder")
}
