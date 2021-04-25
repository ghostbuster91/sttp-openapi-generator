package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.ImportRegistry._

import scala.meta._

object CirceTypeProvider extends JsonTypeProvider {
  val JsonTpe = registerExternalTpe(q"import _root_.io.circe.Json")
  override val AnyType: IM[Type.Name] = JsonTpe
  val ErrorType: IM[Type.Name] = registerExternalTpe(
    q"import _root_.io.circe.{Error => CirceError}"
  )
  val EncoderTpe = registerExternalTpe(q"import _root_.io.circe.Encoder")
  val DecoderTpe = registerExternalTpe(q"import _root_.io.circe.Decoder")
  val HCursoerTpe = registerExternalTpe(q"import _root_.io.circe.HCursor")
  val DecodingFailureTpe = registerExternalTpe(
    q"import _root_.io.circe.DecodingFailure"
  )
  val DecodingResultTpe = registerExternalTpe(
    q"import _root_.io.circe.Decoder.Result"
  )
  val JsonObjectTpe = registerExternalTpe(q"import _root_.io.circe.JsonObject")
}
