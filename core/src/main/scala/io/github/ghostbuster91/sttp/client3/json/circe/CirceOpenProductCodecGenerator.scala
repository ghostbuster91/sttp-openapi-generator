package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.model._

import scala.meta._

private[circe] class CirceOpenProductCodecGenerator {

  def generate(openProduct: OpenProduct): IM[List[Stat]] =
    for {
      decoder <- decoderForSchema(openProduct)
      encoder <- encoderForSchema(openProduct)
    } yield q"""
    $decoder
    $encoder
    """.stats

  private def encoderForSchema(openProduct: OpenProduct): IM[Defn.Val] =
    for {
      encoderTpe <- CirceTypeProvider.EncoderTpe
      jsonTpe <- CirceTypeProvider.AnyType
    } yield {
      val resultClassType = openProduct.name.typeName
      val encoderName = openProduct.name.asPrefix("Encoder")
      val encodedVarName = openProduct.name.toVar
      val encoderInit = init"${t"$encoderTpe[$resultClassType]"}()"
      q"""
    implicit val $encoderName: $encoderTpe[$resultClassType] = 
      new $encoderInit {
        override def apply($encodedVarName: $resultClassType): $jsonTpe =
         ${baseEncoderApplication(openProduct)}
            .apply($encodedVarName)
            .deepMerge(
              Encoder.encodeMap[String, $jsonTpe].apply($encodedVarName._additionalProperties)
            )
      }
    """
    }

  private def baseEncoderApplication(openProduct: OpenProduct) = {
    val productEncoder = Term.Name(s"forProduct${openProduct.properties.size}")
    val encoderTypes =
      openProduct.name.typeName +: openProduct.properties.values.toList
    val propNames = openProduct.properties.keys.toList
    val extractors = propNames.map(p => q"p.${p.term}")
    val prodKeys = propNames.map(n => Lit.String(n.v))
    q"Encoder.$productEncoder[..$encoderTypes](..$prodKeys)(p => (..$extractors))"
  }

  private def decoderForSchema(openProduct: OpenProduct): IM[Defn.Val] =
    for {
      decoderTpe <- CirceTypeProvider.DecoderTpe
      hCursor <- CirceTypeProvider.HCursorTpe
      decoderRes <- CirceTypeProvider.DecodingResultTpe
      jsonObjectTpe <- CirceTypeProvider.JsonObjectTpe
    } yield {
      val resultClassType = openProduct.name.typeName
      val decoderInit = init"${t"$decoderTpe[$resultClassType]"}()"
      val decoderName = p"${openProduct.name.asPrefix("Decoder")}"
      q"""implicit val $decoderName: $decoderTpe[$resultClassType] = 
          new $decoderInit {
            override def apply(c: $hCursor): $decoderRes[$resultClassType] = 
              ${decoderBody(openProduct, jsonObjectTpe)}
          }
    """
    }

  private def decoderBody(
      openProduct: OpenProduct,
      jsonObjectTpe: Type.Name
  ): Term = {
    val knownProps = openProduct.properties.map { case (k, v) =>
      decodeProperty(k, v)
    }.toList
    val allProps = knownProps :+ ForCompStatement(
      enumerator"additionalProperties <- c.as[$jsonObjectTpe]",
      openProduct.properties.keys.toList
        .foldLeft(q"additionalProperties.toMap": Term)((acc, item) =>
          filterOutProperty(acc, item)
        )
    )
    q"""for {
        ..${allProps.map(_.forStat)}
    } yield ${openProduct.name.term}(..${allProps.map(_.bind)})"""
  }

  private def filterOutProperty(
      source: Term,
      propertyName: PropertyName
  ): Term =
    q"$source.filterKeys(_ != ${propertyName.v})"

  private def decodeProperty(name: PropertyName, pType: Type) =
    ForCompStatement(
      enumerator"${name.patVar} <- c.downField(${name.v}).as[$pType]",
      name.term
    )
}

case class ForCompStatement(forStat: Enumerator.Generator, bind: Term)
