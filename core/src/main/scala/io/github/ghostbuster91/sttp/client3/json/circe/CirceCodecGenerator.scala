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
      products: List[Product]
  ): IM[Source] = {
    val coproductGen = new CirceCoproductCodecGenerator()
    val openProductGen = new CirceOpenProductCodecGenerator()
    val productGen = new CirceProductCodecGenerator()
    for {
      productCodecs <- products
        .collect { case p: Product.Regular => p }
        .sortBy(_.name)
        .traverse(productGen.generate)
        .map(_.flatten)
      openProductCodecs <- products
        .collect { case p: Product.Open => p }
        .sortBy(_.name)
        .traverse(openProductGen.generate)
        .map(_.flatten)
      coproductCodecs <- coproducts
        .sortBy(_.name)
        .traverse(coproductGen.generate)
        .map(_.flatten)
      enumCodecs <- enums
        .sortBy(_.name)
        .traverse(CirceEnumCodecGenerator.generate)
        .map(_.flatten)
    } yield source"""
    import _root_.sttp.client3.circe.SttpCirceApi

    trait CirceCodecs extends SttpCirceApi {
        ..$enumCodecs
        ..$productCodecs
        ..$openProductCodecs
        ..$coproductCodecs
    }
    object CirceCodecs extends CirceCodecs
    """
  }
}
