package io.github.ghostbuster91.sttp.client3
import scala.meta._

object EnumGenerator {
  def generate(schemas: Map[String, SafeSchema]): List[Stat] =
    collectEnums(schemas, Nil).flatMap(enumToSealedTraitDef)

  private def enumToSealedTraitDef(enum: Enum) = {
    val objs = enum.values.map { value =>
      val paranetInit = init"${Type.Name(enum.name)}()"
      q"case object ${Term.Name(value.name)} extends $paranetInit{}"
    }
    source"""sealed trait ${Type.Name(enum.name)}
    object ${Term.Name(enum.name)} {
          ..$objs
      }
    """.stats
  }

  private def collectEnums(
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
            List(Enum(path, list.map(v => EnumValue(v)), EnumType.EString))
          case Nil => Nil
        }
      case _: SafeIntegerSchema =>
        schema.enum match {
          case list if list.nonEmpty =>
            List(Enum(path, list.map(v => EnumValue(v)), EnumType.EInt))
          case Nil => Nil
        }
      case other => List.empty
    }
}
