package io.github.ghostbuster91.sttp.client3.model

import scala.meta._

case class OpenProduct(
    name: ClassName,
    properties: List[ParameterRef],
    additionalProperties: ParameterRef
)

case class ClassName(v: String) {
  def term: Term.Name = Term.Name(v)
  def toFqnType(coproduct: Coproduct): Type =
    t"${coproduct.name.term}.$typeName"
  def typeName: Type.Name = Type.Name(v)
  def asPattern: Pat = p"${toParam.patVar}: $typeName"
  def toParam: ParameterName = ParameterName(v)
}

case class ParameterRef(
    tpe: Type,
    paramName: ParameterName,
    defaultValue: Option[Term]
) {
  def asParam: Term.Param = defaultValue match {
    case Some(value) => param"${paramName.term} : $tpe = $value"
    case None        => param"${paramName.term} : $tpe"
  }

  def asDef: Decl.Def = q"def ${paramName.term}: $tpe"

  def asOption: ParameterRef =
    copy(
      tpe = t"Option[$tpe]",
      defaultValue = defaultValue.map(d => q"Some($d)")
    )

  def withName(name: String): ParameterRef =
    copy(paramName = ParameterName(name))
}

object ParameterRef {
  def apply(v: String, defaultValue: Option[Term]): ParameterRef =
    new ParameterRef(
      Type.Name(v),
      ParameterName(v),
      defaultValue
    )
  def apply(tpe: Type.Name): ParameterRef =
    new ParameterRef(tpe, ParameterName(tpe.value), None)
}

case class ParameterName(v: String) {
  def patVar: Pat = Pat.Var(Term.Name(v))
  def term: Term.Name = Term.Name(v)
  def asPrefix(postfix: String): Pat.Var =
    Pat.Var(Term.Name(s"$v$postfix"))
}
object ParameterName {
  def apply(v: String): ParameterName = new ParameterName(uncapitalized(v))

  private def uncapitalized(v: String): String =
    v.take(1).toLowerCase() + v.drop(1)
}
