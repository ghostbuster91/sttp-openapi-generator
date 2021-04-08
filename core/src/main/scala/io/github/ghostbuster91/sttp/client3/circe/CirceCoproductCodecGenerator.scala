package io.github.ghostbuster91.sttp.client3.circe

import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.ImportRegistry
import scala.meta._

class CirceCoproductCodecGenerator(ir: ImportRegistry) {

  def generate(coproduct: Coproduct): List[Stat] =
    q"""
    ..${decoder(coproduct)}
    ..${encoder(coproduct)}
    """.stats

  private def encoder(coproduct: Coproduct) =
    coproduct.discriminator.filter(_.mapping.nonEmpty).map { discriminator =>
      ir.registerImport(q"import _root_.io.circe.HCursor")
      ir.registerImport(q"import _root_.io.circe.Json")
      ir.registerImport(q"import _root_.io.circe.DecodingFailure")
      ir.registerImport(q"import _root_.io.circe.Decoder.Result")
      val coproductType = coproduct.typeName
      val encoderName = coproduct.asPrefix("Encoder")
      val cases = encoderCases(discriminator)
      q"""implicit val $encoderName: Encoder[$coproductType] = new Encoder[$coproductType] {
            override def apply(${coproduct.toVar}: $coproductType): Json = 
              ${coproduct.toVar} match {
                ..case $cases
            }
        }
        """
    }

  private def decoder(coproduct: Coproduct) =
    coproduct.discriminator.filter(_.mapping.nonEmpty).map { discriminator =>
      val cases = decoderCases(discriminator)
      val coproductType = coproduct.typeName
      val decoderName = coproduct.asPrefix("Decoder")
      val dscType = discriminator match {
        case _: Discriminator.StringDsc        => t"String"
        case _: Discriminator.IntDsc           => t"Int"
        case Discriminator.EnumDsc(_, enum, _) => enum.typeName
      }

      q"""implicit val $decoderName: Decoder[$coproductType] = new Decoder[$coproductType] {
            override def apply(c: HCursor): Result[$coproductType] = 
              c.downField(${discriminator.fieldName}).as[$dscType].flatMap {
                ..case $cases
            }
        }"""
    }

  private def encoderCases(
      discriminator: Discriminator[_]
  ): List[Case] = {
    val encoderCasesForTypes = (discriminator match {
      case Discriminator.StringDsc(_, mapping) =>
        mapping.values.map(clazzToEncoderCase)
      case Discriminator.IntDsc(_, mapping) =>
        mapping.values.map(clazzToEncoderCase)
      case Discriminator.EnumDsc(_, _, mapping) =>
        mapping.values.map(clazzToEncoderCase)
    })
    encoderCasesForTypes.map { case EncoderCase(when, child) =>
      p"case $when => Encoder[${child.typeName}].apply(${child.toVar})"
    }.toList
  }

  private def clazzToEncoderCase(clazz: ClassName) =
    EncoderCase(clazz.asParam, clazz)

  private def decoderCases(discriminator: Discriminator[_]): List[Case] = {
    val mappedCases = discriminator match {
      case Discriminator.StringDsc(_, mapping) =>
        mapping.map { case (k, v) => decoderCaseForString(k, v) }.toList
      case Discriminator.IntDsc(_, mapping) =>
        mapping.map { case (k, v) => decoderCaseForInt(k, v) }.toList
      case Discriminator.EnumDsc(_, enum, mapping) =>
        mapping.map { case (k, v) => decoderCaseForEnum(enum)(k, v) }.toList
    }
    mappedCases :+ decoderOtherwiseCase()
  }

  private def decoderCaseForString(discValue: String, child: ClassName) =
    p"case $discValue => Decoder[${child.typeName}].apply(c)"

  private def decoderCaseForInt(discValue: Int, child: ClassName) =
    p"case $discValue => Decoder[${child.typeName}].apply(c)"

  private def decoderCaseForEnum(
      enum: Enum
  )(discValue: EnumValue, child: ClassName) = {
    val evPatVar = p"${enum.term}.${discValue.simpleName}"
    p"case $evPatVar => Decoder[${child.typeName}].apply(c)"
  }

  private def decoderOtherwiseCase() = {
    val otherTerm = Term.Name("other")
    val otherTermBind = Pat.Var(otherTerm)
    p"""case $otherTermBind => Left(DecodingFailure("Unexpected value for coproduct:" + $otherTerm, Nil))"""
  }
}

case class EncoderCase(
    when: Pat,
    child: ClassName
)

case class DecoderCase(
    when: Pat,
    discDecoderType: Type,
    discValue: Term,
    child: ClassName
)
