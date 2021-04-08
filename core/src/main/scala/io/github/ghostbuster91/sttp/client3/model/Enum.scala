package io.github.ghostbuster91.sttp.client3.model

import scala.meta._

sealed trait Enum {
  def values: List[EnumValue]
  def name: ClassName

  def typeName: Type.Name = name.typeName
  def term = name.term
  def asPrefix(postfix: String) = name.asPrefix(postfix)
}
object Enum {
  case class StringEnum(name: ClassName, values: List[EnumValue.StringEv])
      extends Enum
  case class IntEnum(name: ClassName, values: List[EnumValue.IntEv])
      extends Enum
}

sealed trait EnumValue {
  def fqnName(enum: Enum): Term
  def simpleName: Term.Name
}
object EnumValue {
  case class StringEv(v: String) extends EnumValue {
    override def fqnName(enum: Enum): Term =
      q"${enum.name.term}.${Term.Name(v.capitalize)}"
    override def simpleName: Term.Name = Term.Name(v.capitalize)
  }
  case class IntEv(v: Int) extends EnumValue {
    override def fqnName(enum: Enum): Term =
      q"${enum.name.term}.${Term.Name(v.toString)}"
    override def simpleName: Term.Name = Term.Name(v.toString)
  }
}
