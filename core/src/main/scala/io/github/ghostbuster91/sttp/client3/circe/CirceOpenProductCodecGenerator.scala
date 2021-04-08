package io.github.ghostbuster91.sttp.client3.circe

import io.github.ghostbuster91.sttp.client3.ImportRegistry
import io.github.ghostbuster91.sttp.client3.model._

import scala.meta._

class CirceOpenProductCodecGenerator(ir: ImportRegistry) {

  def generate(openProduct: OpenProduct): List[Stat] =
    q"""
    ${decoderForSchema(openProduct)}
    ${encoderForSchema(openProduct)}
    """.stats

  private def encoderForSchema(openProduct: OpenProduct) = {
    val resultClassName = openProduct.name.v
    val resultClassType = openProduct.name.asType
    val resultUncapitalized = uncapitalized(resultClassName)
    val encoderName = openProduct.name.asPrefix("Encoder")
    val encodedVarName = Term.Name(resultUncapitalized)
    q"""
    implicit val $encoderName: Encoder[$resultClassType] = 
      new Encoder[$resultClassType] {
        override def apply($encodedVarName: $resultClassType): Json =
         ${baseEncoderApplication(openProduct)}
            .apply($encodedVarName)
            .deepMerge(
              Encoder.encodeMap[String, Json].apply($encodedVarName._additionalProperties)
            )
      }
    """
  }

  private def baseEncoderApplication(openProduct: OpenProduct) = {
    val productEncoder = Term.Name(s"forProduct${openProduct.properties.size}")
    val encoderTypes =
      openProduct.name.asType +: openProduct.properties.values.toList
    val propNames = openProduct.properties.keys.toList
    val extractors = propNames.map(p => q"p.${p.term}")
    val prodKeys = propNames.map(n => Lit.String(n.v))
    q"Encoder.$productEncoder[..$encoderTypes](..$prodKeys)(p => (..$extractors))"
  }

  private def decoderForSchema(openProduct: OpenProduct) = {
    ir.registerImport(q"import _root_.io.circe.HCursor")
    ir.registerImport(q"import _root_.io.circe.Decoder.Result")
    val resultClassType = openProduct.name.asType
    val decoderName = p"${openProduct.name.asPrefix("Decoder")}"
    q"""implicit val $decoderName: Decoder[$resultClassType] = 
          new Decoder[$resultClassType] {
            override def apply(c: HCursor): Result[$resultClassType] = 
              ${decoderBody(openProduct)}
          }
    """
  }

  def decoderBody(openProduct: OpenProduct) = {
    ir.registerImport(q"import _root_.io.circe.JsonObject")
    val knownProps = openProduct.properties.map { case (k, v) =>
      decodeProperty(k, v)
    }.toList
    val allProps = knownProps :+ ForCompStatement(
      enumerator"additionalProperties <- c.as[JsonObject]",
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

  def uncapitalized(name: String): String =
    name.take(1).toLowerCase() + name.drop(1) //package level rich function?
  //or modelGenerator returns richer object with such method
}

case class ForCompStatement(forStat: Enumerator.Generator, bind: Term)
