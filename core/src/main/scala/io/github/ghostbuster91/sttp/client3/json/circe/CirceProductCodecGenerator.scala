package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.ImportRegistry.IM
import io.github.ghostbuster91.sttp.client3.model.Product

import scala.meta._

class CirceProductCodecGenerator {
  def generate(product: Product.Regular): IM[List[Stat]] =
    for {
      dec <- decoder(product)
      enc <- encoder(product)
    } yield q"""
    $dec
    $enc
    """.stats

  private def encoder(product: Product.Regular) =
    CirceTypeProvider.EncoderTpe.map { encoderTpe =>
      val resultClassType = product.name.typeName
      val encoderName = product.name.toParam.asPrefix("Encoder")
      product.properties match {
        case Nil =>
          throw new IllegalArgumentException(
            s"Property list cann be empty $product"
          )
        case head :: Nil =>
          val propNames = head.paramName
          q"""implicit val $encoderName: $encoderTpe[$resultClassType] = 
                Encoder.forProduct1(${Lit.String(
            propNames.v
          )})(p => p.${propNames.term})"""
        case props @ _ :: _ =>
          val productEncoder = Term.Name(s"forProduct${props.size}")
          val propNames = props.map(_.paramName)
          val extractors = propNames.map(p => q"p.${p.term}")
          q"""implicit val $encoderName: $encoderTpe[$resultClassType] = 
                Encoder.$productEncoder(..${propNames.map(p =>
            Lit.String(p.v)
          )})(p => (..$extractors))"""
      }
    }

  private def decoder(product: Product.Regular) =
    CirceTypeProvider.DecoderTpe.map { decoderTpe =>
      val resultClassType = product.name.typeName
      val decoderName = product.name.toParam.asPrefix("Decoder")
      val productDecoder = Term.Name(s"forProduct${product.properties.size}")
      val productElements =
        product.properties.map(p => Lit.String(p.paramName.v))
      q"""implicit val $decoderName: $decoderTpe[$resultClassType] = 
                Decoder.$productDecoder(..$productElements)(${product.name.term}.apply)"""
    }
}
