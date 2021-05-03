package io.github.ghostbuster91.sttp.client3.json.circe

import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.json._
import scala.meta._

class CirceCodecGenerator() extends JsonCodecGenerator {
  def generate(
      enums: List[Enum],
      coproducts: List[Coproduct],
      openProducts: List[Product.Open]
  ): IM[Source] = {
    val coproductGen = new CirceCoproductCodecGenerator()
    val openProductGen = new CirceOpenProductCodecGenerator()
    for {
      openProductCodecs <- openProducts
        .traverse(openProductGen.generate)
        .map(_.flatten)
      coproductCodecs <- coproducts
        .traverse(coproductGen.generate)
        .map(_.flatten)
      enumCodecs <- enums
        .traverse(CirceEnumCodecGenerator.generate)
        .map(_.flatten)
    } yield source"""
    import _root_.io.circe.generic.AutoDerivation
    import _root_.sttp.client3.circe.SttpCirceApi

    trait CirceCodecs extends AutoDerivation with SttpCirceApi {
        ..$enumCodecs
        ..$coproductCodecs
        ..$openProductCodecs
    }"""
  }
}
