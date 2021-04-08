package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3._
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.json._
import scala.meta._

class CirceCodecGenerator(ir: ImportRegistry) extends JsonCodecGenerator {
  def generate(
      enums: List[Enum],
      coproducts: List[Coproduct],
      openProducts: List[OpenProduct]
  ): Source = {
    val coproductGen = new CirceCoproductCodecGenerator(ir)
    val openProductGen = new CirceOpenProductCodecGenerator(ir)
    val enumCodecs = enums.flatMap(CirceEnumCodecGenerator.generate)
    val coproductCodecs = coproducts.flatMap(coproductGen.generate)
    val openProductCodecs = openProducts.flatMap(openProductGen.generate)

    source"""import _root_.io.circe.Decoder
    import _root_.io.circe.Encoder
    import _root_.io.circe.generic.AutoDerivation
    import _root_.sttp.client3.circe.SttpCirceApi

    trait CirceCodecs extends AutoDerivation with SttpCirceApi {
        ..$enumCodecs
        ..$coproductCodecs
        ..$openProductCodecs
    }"""
  }
}
