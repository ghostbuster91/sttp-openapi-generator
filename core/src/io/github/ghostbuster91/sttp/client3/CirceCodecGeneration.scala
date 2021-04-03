package io.github.ghostbuster91.sttp.client3

import scala.meta._

object CirceCodecGeneration {
  def generate(enums: List[Enum]): Source = {
    val decoders = enums.map(decoder)
    val encoders = enums.map(encoder)
    source"""import _root_.io.circe.Decoder
    import _root_.io.circe.Encoder
    import _root_.io.circe.generic.AutoDerivation
    import _root_.sttp.client3.circe.SttpCirceApi

    trait CirceCodecs extends AutoDerivation with SttpCirceApi {
        ..$decoders
        ..$encoders
    }"""
  }

  private def encoder(enum: Enum) = {
    val enumType = Type.Name(enum.name)
    val encoderName = Pat.Var(Term.Name(s"${enum.uncapitalizedName}Encoder"))
    val cases = encoderCases(enum)
    q"""
    implicit val $encoderName: Encoder[$enumType]  = ${baseEncoder(enum)}.contramap {
        ..case $cases
    }
    """
  }

  private def encoderCases(enum: Enum): List[Case] =
    enum.values.map { ev =>
      val pThen = evToLit(ev)
      val enumValueName = ev.name
      val pWhen = p"${Term.Name(enum.name)}.$enumValueName"
      p"case $pWhen => $pThen"
    }

  private def decoder(enum: Enum) = {
    val cases = decoderCases(enum)
    val enumType = Type.Name(enum.name)
    val decoderName = Pat.Var(Term.Name(s"${enum.uncapitalizedName}Decoder"))
    q"""
    implicit val $decoderName: Decoder[$enumType]  = ${baseDecoder(enum)}.emap {
        ..case $cases
    }
    """
  }

  private def decoderCases[T](enum: Enum): List[Case] = {
    val cases = enum.values.map { ev =>
      val pWhen = p"${evToLit(ev)}"
      val enumValueName = ev.name
      val pThen = q"${Term.Name(enum.name)}.$enumValueName"
      p"case $pWhen => Right($pThen)"
    }
    cases :+ decoderOtherwiseCase()
  }

  private def decoderOtherwiseCase() = {
    val otherTerm = Term.Name("other")
    val otherTermBind = Pat.Var(otherTerm)
    p"""case $otherTermBind => Left("Unexpected value for enum:" + $otherTerm)"""
  }

  private def evToLit[T](ev: EnumValue): Lit =
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
