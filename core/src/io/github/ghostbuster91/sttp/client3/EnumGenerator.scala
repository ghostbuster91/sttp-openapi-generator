package io.github.ghostbuster91.sttp.client3
import scala.meta._

object EnumGenerator {
  def generate(schemas: Map[String, SafeSchema]): List[Stat] =
    collectEnums(schemas, Nil).flatMap(enumToSealedTraitDef)

  private def enumToSealedTraitDef(enum: Enum) = {
    val objs =
      enum.values.map(value =>
        Defn.Object(
          List(Mod.Case()),
          Term.Name(value.name),
          Template(
            early = Nil,
            inits =
              List(Init(Type.Name(enum.name), Name.Anonymous(), List.empty)),
            self = Self(
              name = Name.Anonymous(),
              decltpe = None
            ),
            stats = Nil
          )
        )
      )
    source"""sealed trait ${Type.Name(enum.name)}
    object ${Term.Name(enum.name)} {
          ..$objs
      }
    """.stats
  }

  private def collectEnums(
      schemas: Map[String, SafeSchema],
      path: List[String]
  ): List[Enum] =
    schemas.flatMap { case (k, v) =>
      collectEnums2(path :+ k, v)
    }.toList

  private def collectEnums2(
      path: List[String],
      schema: SafeSchema
  ): List[Enum] =
    schema match {
      case os: SafeObjectSchema =>
        os.properties.toList.flatMap { case (k, v) =>
          collectEnums2(path :+ k, v)
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
