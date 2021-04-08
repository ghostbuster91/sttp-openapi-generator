package io.github.ghostbuster91.sttp.client3.model

import scala.meta._

case class OpenProduct(
    name: ClassName,
    properties: Map[PropertyName, Type]
)
case class ClassName(v: String) {
  def asPrefix(postfix: String) =
    Pat.Var(Term.Name(s"$uncapitalized$postfix")) //p"$uncapitalized$postfix"
  private def uncapitalized: String =
    v.take(1).toLowerCase() + v.drop(1)

  def term = Term.Name(v)
  def toVar: Term.Name =
    Term.Name(uncapitalized)
  def toFqnType(coproduct: Coproduct): Type =
    t"${coproduct.name.term}.${Type.Name(v)}"
  def typeName: Type.Name = Type.Name(v)

  def asParam: Pat =
    p"${Pat.Var(toVar)}: $typeName"
}

case class PropertyName(v: String) {
  def patVar: Pat = Pat.Var(Term.Name(v))
  def term: Term.Name = Term.Name(v)
}
