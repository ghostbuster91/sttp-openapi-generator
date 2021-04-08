package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._

object EnumCollector {

  def collectEnums(schemas: Map[String, SafeSchema]): List[Enum] =
    schemas.flatMap { case (k, v) =>
      collectEnumsFromSingleSchema(k, v)
    }.toList

  private def collectEnumsFromSingleSchema(
      schemaName: String,
      schema: SafeSchema
  ): Option[Enum] =
    schema match {
      case ss: SafeStringSchema if ss.isEnum =>
        Some(
          Enum.StringEnum(
            ClassName(schemaName.capitalize),
            ss.enum.map(v => EnumValue.StringEv(v.asInstanceOf[String]))
          )
        )
      case si: SafeIntegerSchema if si.isEnum =>
        Some(
          Enum.IntEnum(
            ClassName(schemaName.capitalize),
            si.enum.map(v => EnumValue.IntEv(v.asInstanceOf[Int]))
          )
        )
      case _ => None
    }
}
