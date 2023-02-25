package io.github.ghostbuster91.sttp.client3.json.circe

import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import io.github.ghostbuster91.sttp.client3.model._

import scala.meta._

private[circe] class CirceCoproductCodecGenerator() {

  def generate(coproduct: Coproduct): IM[List[Stat]] = for {
    e <- encoder(coproduct)
    d <- decoder(coproduct)
  } yield q"""
    $d
    $e
    """.stats

  private def encoder(coproduct: Coproduct): IM[Defn.Val] = {
    val coproductType = coproduct.typeName
    val encoderName = coproduct.asPrefix("Encoder")
    for {
      encoderTpe <- CirceTypeProvider.EncoderTpe
    } yield {
      val cases = encoderCases(coproduct)
      val encoderInit = init"${t"$encoderTpe[$coproductType]"}()"
      q"""implicit lazy val $encoderName: $encoderTpe[$coproductType] = Encoder.instance {
            ..case $cases
        }
        """
    }
  }

  private def decoder(coproduct: Coproduct): IM[Defn.Val] =
    coproduct.discriminator match {
      case Some(discriminator) if discriminator.mapping.nonEmpty =>
        decoderForCoproductWithMappedDiscriminator(coproduct, discriminator)
      case Some(discriminator: Discriminator.StringDsc) =>
        decoderForCoproductWithMappedDiscriminator(coproduct, discriminator)
      case _ =>
        val coproductType = coproduct.typeName
        val decoderName = coproduct.asPrefix("Decoder")
        for {
          decoderTpe <- CirceTypeProvider.DecoderTpe
        } yield q"""implicit lazy val $decoderName: $decoderTpe[$coproductType] = List[$decoderTpe[$coproductType]](..${decoderCases(
            coproduct
          )}).reduceLeft(_ or _)"""
    }

  private def decoderForCoproductWithMappedDiscriminator[T](
      coproduct: Coproduct,
      discriminator: Discriminator[T]
  ): IM[Defn.Val] = {
    val coproductType = coproduct.typeName
    val decoderName = coproduct.asPrefix("Decoder")
    for {
      hCursorTpe <- CirceTypeProvider.HCursorTpe
      decoderTpe <- CirceTypeProvider.DecoderTpe
      failureTpe <- CirceTypeProvider.DecodingFailureTpe
      resultTpe <- CirceTypeProvider.DecodingResultTpe
    } yield {
      val cases =
        decoderCasesWithMapping(discriminator, failureTpe)
      val dscType = discriminator match {
        case _: Discriminator.StringDsc        => t"String"
        case _: Discriminator.IntDsc           => t"Int"
        case Discriminator.EnumDsc(_, enum, _) => enum.typeName
      }
      val decoderInit = init"${t"$decoderTpe[$coproductType]"}()"
      q"""implicit lazy val $decoderName: $decoderTpe[$coproductType] = new $decoderInit {
            override def apply(c: $hCursorTpe): $resultTpe[$coproductType] =
              c.downField(${discriminator.fieldName}).as[$dscType].flatMap {
                ..case $cases
            }
        }"""
    }
  }

  private def implicitMappingForCoproductDiscriminator(
      coproduct: Coproduct
  ): Map[String, ClassName] =
    coproduct.childs.toList.map { child: ClassName =>
      child.typeName.value -> child
    }.toMap

  private def encoderCases(coproduct: Coproduct) =
    coproduct.childs.toList
      .sortBy(_.v)
      .map { child =>
        p"case ${child.asPattern} => Encoder[${child.typeName}].apply(${child.toParam.term})"
      }

  private def decoderCases(coproduct: Coproduct) =
    coproduct.childs.toList
      .sortBy(_.v)
      .map(child =>
        q"Decoder[${child.typeName}].asInstanceOf[Decoder[${coproduct.typeName}]]"
      )

  private def encoderCasesWithMapping(
      discriminator: Discriminator[_]
  ): List[Case] = {
    val encoderCasesForTypes = discriminator match {
      case Discriminator.StringDsc(_, mapping) =>
        mapping.values.map(clazzToEncoderCase)
      case Discriminator.IntDsc(_, mapping) =>
        mapping.values.map(clazzToEncoderCase)
      case Discriminator.EnumDsc(_, _, mapping) =>
        mapping.values.map(clazzToEncoderCase)
    }
    encoderCasesForTypes.map { case EncoderCase(when, child) =>
      p"case $when => Encoder[${child.typeName}].apply(${child.toParam.term})"
    }.toList
  }

  private def clazzToEncoderCase(clazz: ClassName) =
    EncoderCase(clazz.asPattern, clazz)

  private def decoderCasesWithMapping(
      discriminator: Discriminator[_],
      failureTpe: Type.Name
  ): List[Case] = {
    val mappedCases = discriminator match {
      case Discriminator.StringDsc(_, mapping) =>
        mapping.toList.sortBy(_._1).map { case (k, v) =>
          decoderCaseForString(k, v)
        }
      case Discriminator.IntDsc(_, mapping) =>
        mapping.toList.sortBy(_._1).map { case (k, v) =>
          decoderCaseForInt(k, v)
        }
      case Discriminator.EnumDsc(_, enum, mapping) =>
        mapping.toList.sortBy(_._1.simpleName.value).map { case (k, v) =>
          decoderCaseForEnum(enum)(k, v)
        }
    }
    mappedCases :+ decoderOtherwiseCase(failureTpe)
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

  private def decoderOtherwiseCase(failureTpe: Type.Name) = {
    val otherTerm = Term.Name("other")
    val otherTermBind = Pat.Var(otherTerm)
    val failureInit =
      q"""${Term.Name(
          failureTpe.value
        )}("Unexpected value for coproduct:" + $otherTerm, Nil)"""
    p"""case $otherTermBind => Left($failureInit)"""
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
