package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.ImportRegistry._

import scala.meta._

object CirceTypeProvider extends JsonTypeProvider {
  val AnyType: IM[Type.Name] =
    registerExternalTpe(q"import _root_.io.circe.Json")
  val ErrorType: IM[Type.Name] = registerExternalTpe(
    q"import _root_.io.circe.{Error => CirceError}"
  )
  val EncoderTpe = registerExternalTpe(q"import _root_.io.circe.Encoder")
  val DecoderTpe = registerExternalTpe(q"import _root_.io.circe.Decoder")
}
