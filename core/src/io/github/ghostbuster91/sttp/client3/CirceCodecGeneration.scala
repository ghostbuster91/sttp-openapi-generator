package io.github.ghostbuster91.sttp.client3

import scala.meta._

object CirceCodecGeneration {
  def generate(enums: List[Enum]): String = {

    /**   implicit val personStatusDecoder: Decoder[PersonStatus] = Decoder.decodeString.emap {
      *    case "happy" => Right(PersonStatus.Happy: PersonStatus)
      *    case "neutral" => Right(PersonStatus.Happy: PersonStatus)
      *    case other => Left(other): Either[String, PersonStatus]
      *  }
      *
      *  implicit val personStatusEncoder: Encoder[PersonStatus] = Encoder.encodeString.contramap {
      *    case PersonStatus.Happy => "happy"
      *    case PersonStatus.Neutral => "neutral"
      *  }
      */
    val decoders = enums.map(decoder)
    q"""package io.github.ghostbuster91.sttp.client3 {

    import io.circe.Decoder

    trait CirceCodecs {
        ..$decoders
    }
    }""".toString()
  }

  private def decoder(enum: Enum) = {
    val cases = decoderCases(enum)
    val enumType = Type.Name(enum.name)
    val decoderName = Pat.Var(Term.Name(s"${enum.uncapitalizedName}Decoder"))
    q"""
    implicit val $decoderName: Decoder[$enumType]  = Decoder.decodeString.emap {
        ..case $cases
    }
    """
  }

  private def decoderCases(enum: Enum): List[Case] = {
    val cases = enum.values.map { ev =>
      val pWhen = p"${ev.rawValue.toString()}"
      val pThen = q"${Term.Name(enum.name)}.${Term.Name(ev.name)}"
      p"case $pWhen => Right($pThen)"
    }
    cases :+ decoderOtherwiseCase()
  }

  private def decoderOtherwiseCase() = {
    val otherTerm = Term.Name("other")
    val otherTermBind = Pat.Var(otherTerm)
    p"""case $otherTermBind => Left("Unexpected value for enum:" + $otherTerm)"""
  }
}
