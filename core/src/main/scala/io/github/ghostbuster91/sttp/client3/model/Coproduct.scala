package io.github.ghostbuster91.sttp.client3.model

case class Coproduct(
    name: String,
    discriminator: Option[Discriminator[_]]
) {

  def uncapitalizedName: String = name.take(1).toLowerCase() + name.drop(1)
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
