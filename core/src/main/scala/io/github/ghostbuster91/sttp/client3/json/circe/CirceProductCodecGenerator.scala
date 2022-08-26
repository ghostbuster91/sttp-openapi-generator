package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.ImportRegistry.IM
import io.github.ghostbuster91.sttp.client3.model.{
  Coproduct,
  Discriminator,
  EnumValue,
  ParameterName,
  Product
}

import scala.meta._

class CirceProductCodecGenerator {
  def generate(
      product: Product.Regular,
      coproducts: List[Coproduct]
  ): IM[List[Stat]] =
    for {
      dec <- decoder(product, coproducts)
      enc <- encoder(product, coproducts)
    } yield q"""
    $dec
    $enc
    """.stats

  private def encoder(product: Product.Regular, coproducts: List[Coproduct]) =
    CirceTypeProvider.EncoderTpe.map { encoderTpe =>
      val resultClassType = product.name.typeName
      val encoderName = product.name.toParam.asPrefix("Encoder")
      val discriminatorProp =
        determineDiscriminator(
          product: Product.Regular,
          coproducts: List[Coproduct]
        )

      product.properties match {
        case Nil =>
          throw new IllegalArgumentException(
            s"Property list can be empty $product"
          )
        case head :: Nil =>
          val propName = head.paramName
          discriminatorProp match {
            case Some((name, value)) if name == propName =>
              q"""implicit lazy val $encoderName: $encoderTpe[$resultClassType] =
                  Encoder.forProduct1(${Lit.String(
                propName.v
              )})(_ => $value)"""
            case _ =>
              q"""implicit lazy val $encoderName: $encoderTpe[$resultClassType] =
                    Encoder.forProduct1(${Lit.String(
                propName.v
              )})(p => p.${propName.term})"""
          }
        case props @ _ :: _ =>
          val productEncoder = Term.Name(s"forProduct${props.size}")
          val propNames = props.map(_.paramName)
          val extractors = propNames.map { p =>
            discriminatorProp match {
              case Some((name, value)) if name == p =>
                q"$value"
              case _ =>
                q"p.${p.term}"
            }
          }
          q"""implicit lazy val $encoderName: $encoderTpe[$resultClassType] =
                Encoder.$productEncoder(..${propNames.map(p =>
            Lit.String(p.v)
          )})(p => (..$extractors))"""
      }
    }

  private def decoder(product: Product.Regular, coproducts: List[Coproduct]) =
    CirceTypeProvider.DecoderTpe.map { decoderTpe =>
      val resultClassType = product.name.typeName
      val decoderName = product.name.toParam.asPrefix("Decoder")
      val discriminatorPropName =
        determineDiscriminator(
          product: Product.Regular,
          coproducts: List[Coproduct]
        ).map(t => t._1)

      val productElements =
        product.properties
          .filterNot(p => discriminatorPropName.contains(p.paramName))
          .map(p => Lit.String(p.paramName.v))

      productElements.size match {
        case 0 =>
          val constDecoder = Term.Name(s"const")
          q"""implicit lazy val $decoderName: $decoderTpe[$resultClassType] =
                Decoder.$constDecoder(${product.name.term}.apply)"""
        case size =>
          val productDecoder = Term.Name(s"forProduct$size")
          q"""implicit lazy val $decoderName: $decoderTpe[$resultClassType] =
                Decoder.$productDecoder(..$productElements)(${product.name.term}.apply)"""
      }
    }

  private def determineDiscriminator(
      product: Product.Regular,
      coproducts: List[Coproduct]
  ): Option[(ParameterName, Lit)] =
    product.parents match {
      case parents if parents.nonEmpty =>
        val parentCoproducts = coproducts.filter { coproduct: Coproduct =>
          parents.contains(coproduct.name)
        }
        val discriminator =
          parentCoproducts.flatMap(cp => cp.discriminator).headOption
        discriminator match {
          case Some(Discriminator.StringDsc(fieldName, mapping)) =>
            (
              product.properties
                .find(p => p.paramName.term.value == fieldName)
                .map(_.paramName),
              mapping
                .find { case (_, className) => className == product.name }
                .map(_._1)
                .map(Lit.String(_))
            ).sequence
          case Some(Discriminator.EnumDsc(fieldName, _, mapping)) =>
            (
              product.properties
                .find(p => p.paramName.term.value == fieldName)
                .map(_.paramName),
              mapping
                .find { case (_, className) => className == product.name }
                .map(_._1)
                .map {
                  case EnumValue.StringEv(v) => Lit.String(v)
                  case EnumValue.IntEv(v)    => Lit.Int(v)
                }
            ).sequence
          case Some(Discriminator.IntDsc(fieldName, mapping)) =>
            (
              product.properties
                .find(p => p.paramName.term.value == fieldName)
                .map(_.paramName),
              mapping
                .find { case (_, className) => className == product.name }
                .map(_._1)
                .map(Lit.Int(_))
            ).sequence
          case None => None
        }
      case _ => None
    }

  private implicit class Tuple2Helper[A, B](
      value: (Option[A], Option[B])
  ) {
    def sequence: Option[(A, B)] = for {
      a <- value._1
      b <- value._2
    } yield (a, b)
  }
}
