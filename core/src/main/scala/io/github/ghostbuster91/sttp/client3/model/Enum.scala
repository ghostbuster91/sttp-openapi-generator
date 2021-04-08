package io.github.ghostbuster91.sttp.client3.model

import scala.meta._

sealed trait Enum {
  def path: String
  def values: List[EnumValue]

  def name: String = path.capitalize
  def uncapitalizedName: String = name.take(1).toLowerCase() + name.drop(1)
}
object Enum {
  case class StringEnum(path: String, values: List[EnumValue.StringEv])
      extends Enum
  case class IntEnum(path: String, values: List[EnumValue.IntEv]) extends Enum
}

sealed trait EnumValue {
  def fqnName(enum: Enum): Term
  def simpleName: Term.Name
}
object EnumValue {
  case class StringEv(v: String) extends EnumValue {
    override def fqnName(enum: Enum): Term =
      q"${Term.Name(enum.name)}.${Term.Name(v.capitalize)}"
    override def simpleName: Term.Name = Term.Name(v.capitalize)
  }
  case class IntEv(v: Int) extends EnumValue {
    override def fqnName(enum: Enum): Term =
      q"${Term.Name(enum.name)}.${Term.Name(v.toString)}"
    override def simpleName: Term.Name = Term.Name(v.toString)
  }
}
