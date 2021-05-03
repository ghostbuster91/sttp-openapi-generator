package io.github.ghostbuster91.sttp.client3.model

import scala.meta.{Pat, Term, Type}

case class Coproduct(
    name: ClassName,
    discriminator: Option[Discriminator[_]]
) {
  def typeName: Type.Name = name.typeName
  def asPrefix(postfix: String): Pat.Var = name.toParam.asPrefix(postfix)
  def toVar: Term.Name = name.toParam.term
}

sealed trait Discriminator[T] {
  def fieldName: String
  def mapping: Map[T, ClassName]
}
object Discriminator {
  case class StringDsc(
      fieldName: String,
      mapping: Map[String, ClassName]
  ) extends Discriminator[String]
  case class IntDsc(
      fieldName: String,
      mapping: Map[Int, ClassName]
  ) extends Discriminator[Int]
  case class EnumDsc(
      fieldName: String,
      enum: Enum,
      mapping: Map[EnumValue, ClassName]
  ) extends Discriminator[EnumValue]
}
