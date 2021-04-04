package io.github.ghostbuster91.sttp.client3.circe

import io.github.ghostbuster91.sttp.client3._
import scala.meta._

class CirceCodecGenerator(ir: ImportRegistry) {
  def generate(enums: List[Enum], coproducts: List[Coproduct]): Source = {
    val coproductGen = new CirceCoproductCodecGenerator(ir)
    val encoders = coproducts.flatMap(coproductGen.encoder) ++ enums.map(
      CirceEnumCodecGenerator.encoder
    )

    val decoders = coproducts.flatMap(coproductGen.decoder) ++ enums.map(
      CirceEnumCodecGenerator.decoder
    )

    source"""import _root_.io.circe.Decoder
    import _root_.io.circe.Encoder
    import _root_.io.circe.generic.AutoDerivation
    import _root_.sttp.client3.circe.SttpCirceApi

    trait CirceCodecs extends AutoDerivation with SttpCirceApi {
        ..$decoders
        ..$encoders
    }"""
  }
}
