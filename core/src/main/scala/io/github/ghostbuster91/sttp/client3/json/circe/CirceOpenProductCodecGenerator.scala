package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.json.circe.CirceOpenProductCodecGenerator.ForCompStatement

import scala.meta._

private[circe] class CirceOpenProductCodecGenerator {

  def generate(openProduct: Product.Open): IM[List[Stat]] =
    for {
      decoder <- decoderForSchema(openProduct)
      encoder <- encoderForSchema(openProduct)
    } yield q"""
    $decoder
    $encoder
    """.stats

  private def encoderForSchema(openProduct: Product.Open): IM[Defn.Val] =
    for {
      encoderTpe <- CirceTypeProvider.EncoderTpe
      jsonTpe <- CirceTypeProvider.AnyType
    } yield {
      val resultClassType = openProduct.name.typeName
      val encoderName = openProduct.name.toParam.asPrefix("Encoder")
      val encodedVarName = openProduct.name.toParam.term
      val encoderInit = init"${t"$encoderTpe[$resultClassType]"}()"
      q"""
    implicit val $encoderName: $encoderTpe[$resultClassType] = 
      new $encoderInit {
        override def apply($encodedVarName: $resultClassType): $jsonTpe =
         ${baseEncoderApplication(openProduct)}
            .apply($encodedVarName)
            .deepMerge(
              Encoder[${openProduct.additionalProperties.tpe}].apply($encodedVarName._additionalProperties)
            )
      }
    """
    }

  private def baseEncoderApplication(openProduct: Product.Open) = {
    val productEncoder = Term.Name(s"forProduct${openProduct.properties.size}")
    val encoderTypes =
      openProduct.name.typeName +: openProduct.properties.map(_.tpe)
    val propNames = openProduct.properties.map(_.paramName)
    val extractors = propNames.map(p => q"p.${p.term}")
    val prodKeys = propNames.map(n => Lit.String(n.v))
    q"Encoder.$productEncoder[..$encoderTypes](..$prodKeys)(p => (..$extractors))"
  }

  private def decoderForSchema(openProduct: Product.Open): IM[Defn.Val] =
    for {
      decoderTpe <- CirceTypeProvider.DecoderTpe
      hCursor <- CirceTypeProvider.HCursorTpe
      decoderRes <- CirceTypeProvider.DecodingResultTpe
    } yield {
      val resultClassType = openProduct.name.typeName
      val decoderInit = init"${t"$decoderTpe[$resultClassType]"}()"
      val decoderName = p"${openProduct.name.toParam.asPrefix("Decoder")}"
      q"""implicit val $decoderName: $decoderTpe[$resultClassType] = 
          new $decoderInit {
            override def apply(c: $hCursor): $decoderRes[$resultClassType] = 
              ${decoderBody(openProduct)}
          }
    """
    }

  private def decoderBody(
      openProduct: Product.Open
  ): Term = {
    val knownProps = openProduct.properties.map(decodeProperty)
    val allProps = knownProps :+ ForCompStatement(
      enumerator"additionalProperties <- c.as[${openProduct.additionalProperties.tpe}]",
      openProduct.properties
        .map(_.paramName)
        .foldLeft(q"additionalProperties": Term)((acc, item) =>
          filterOutProperty(acc, item)
        )
    )
    q"""for {
        ..${allProps.map(_.forStat)}
    } yield ${openProduct.name.term}(..${allProps.map(_.bind)})"""
  }

  private def filterOutProperty(
      source: Term,
      propertyName: ParameterName
  ): Term =
    q"$source.filterKeys(_ != ${propertyName.v})"

  private def decodeProperty(property: ParameterRef) = {
    val propertyName = property.paramName
    ForCompStatement(
      enumerator"${propertyName.patVar} <- c.downField(${propertyName.v}).as[${property.tpe}]",
      propertyName.term
    )
  }

}

private object CirceOpenProductCodecGenerator {
  private case class ForCompStatement(forStat: Enumerator.Generator, bind: Term)
}
