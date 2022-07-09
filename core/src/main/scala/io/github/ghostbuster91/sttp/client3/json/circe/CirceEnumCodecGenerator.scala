package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import scala.meta._

private[circe] object CirceEnumCodecGenerator {
  def generate(enum: Enum): IM[List[Stat]] =
    for {
      d <- decoder(enum)
      e <- encoder(enum)
    } yield q"""
    $d
    $e
    """.stats

  private def encoder(enum: Enum): IM[Defn.Val] =
    CirceTypeProvider.EncoderTpe
      .map { encoderTpe =>
        val encoderName = enum.asPrefix("Encoder")
        val cases = encoderCases(enum)
        q"""
          implicit lazy val $encoderName: $encoderTpe[${enum.typeName}]  = ${baseEncoder(
          enum
        )}.contramap {
              ..case $cases
          }
          """
      }

  private def decoder(enum: Enum): IM[Defn.Val] =
    CirceTypeProvider.DecoderTpe
      .map { decoderTpe =>
        val cases = decoderCases(enum)
        val decoderName = enum.asPrefix("Decoder")
        q"""
          implicit lazy val $decoderName: $decoderTpe[${enum.typeName}]  = ${baseDecoder(
          enum
        )}.emap {
              ..case $cases
          }
          """
      }

  private def encoderCases(enum: Enum): List[Case] =
    enum.values.sortBy(_.simpleName.value).map { ev =>
      val pThen = evToLit(ev)
      val pWhen = p"${enum.name.term}.${ev.simpleName}"
      p"case $pWhen => $pThen"
    }

  private def decoderCases(enum: Enum): List[Case] = {
    val cases = enum.values.sortBy(_.simpleName.value).map { ev =>
      val pWhen = p"${evToLit(ev)}"
      p"case $pWhen => Right(${ev.fqnName(enum)})"
    }
    cases :+ decoderOtherwiseCase()
  }

  private def decoderOtherwiseCase() = {
    val otherTerm = Term.Name("other")
    val otherTermBind = Pat.Var(otherTerm)
    p"""case $otherTermBind => Left("Unexpected value for enum:" + $otherTerm)"""
  }

  private def evToLit(ev: EnumValue): Lit =
    ev match {
      case EnumValue.StringEv(v) => Lit.String(v)
      case EnumValue.IntEv(v)    => Lit.Int(v)
    }

  private def baseDecoder(enum: Enum) =
    enum match {
      case _: Enum.StringEnum => q"Decoder.decodeString"
      case _: Enum.IntEnum    => q"Decoder.decodeInt"
    }

  private def baseEncoder(enum: Enum) =
    enum match {
      case _: Enum.StringEnum => q"Encoder.encodeString"
      case _: Enum.IntEnum    => q"Encoder.encodeInt"
    }
}
