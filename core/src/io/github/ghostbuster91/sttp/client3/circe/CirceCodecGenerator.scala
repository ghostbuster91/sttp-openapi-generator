package io.github.ghostbuster91.sttp.client3.circe

import io.github.ghostbuster91.sttp.client3._
import scala.meta._

class CirceCodecGenerator(ir: ImportRegistry) {
  def generate(enums: List[Enum], coproducts: List[Coproduct]): Source = {
    val coproductGen = new CirceCoproductCodecGenerator(ir)
    val enumCodecs = enums.flatMap { e =>
      q"""
        ${CirceEnumCodecGenerator.decoder(e)}
        ${CirceEnumCodecGenerator.encoder(e)}
        """.stats
    }

    val coproductCodecs = coproducts.flatMap { cp =>
      (coproductGen.decoder(cp), coproductGen.encoder(cp)) match {
        case (Some(de), Some(en)) =>
          q"""
            $de
            $en
        """.stats
        case _ => List.empty
      }
    }

    source"""import _root_.io.circe.Decoder
    import _root_.io.circe.Encoder
    import _root_.io.circe.generic.AutoDerivation
    import _root_.sttp.client3.circe.SttpCirceApi

    trait CirceCodecs extends AutoDerivation with SttpCirceApi {
        ..$enumCodecs
        ..$coproductCodecs
    }"""
  }
}
