package io.github.ghostbuster91.sttp.client3

object EnumCollector {

  def collectEnums(
      schemas: Map[String, SafeSchema],
      path: List[String],
  ): List[Enum] =
    schemas.flatMap { case (k, v) =>
      collectEnumsFromSingleSchema(path :+ k, v)
    }.toList

  private def collectEnumsFromSingleSchema(
      path: List[String],
      schema: SafeSchema,
  ): List[Enum] =
    schema match {
      case os: SafeObjectSchema =>
        os.properties.toList.flatMap { case (k, v) =>
          collectEnumsFromSingleSchema(path :+ k, v)
        }
      case _: SafeStringSchema =>
        schema.enum match {
          case list if list.nonEmpty =>
            List(
              Enum.StringEnum(
                path,
                list.map(v => EnumValue.StringEv(v.asInstanceOf[String])),
              ),
            )
          case Nil => Nil
        }
      case _: SafeIntegerSchema =>
        schema.enum match {
          case list if list.nonEmpty =>
            List(
              Enum.IntEnum(
                path,
                list.map(v => EnumValue.IntEv(v.asInstanceOf[Int])),
              ),
            )
          case Nil => Nil
        }
      case _ => List.empty
    }
}
