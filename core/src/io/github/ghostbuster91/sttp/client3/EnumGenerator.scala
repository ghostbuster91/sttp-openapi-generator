package io.github.ghostbuster91.sttp.client3
import scala.meta._

object EnumGenerator {
  def generate(schemas: Map[String, SafeSchema]): List[EnumDef] =
    collectEnums(schemas, Nil).map(enumToSealedTraitDef)

  private def enumToSealedTraitDef(enum: Enum) = {
    val name = enum.path.takeRight(2).map(_.capitalize).mkString
    val objs =
      enum.values.map(n =>
        Defn.Object(
          List(Mod.Case()),
          Term.Name(n.toString.capitalize),
          Template(
            early = Nil,
            inits = List(Init(Type.Name(name), Name.Anonymous(), List.empty)),
            self = Self(
              name = Name.Anonymous(),
              decltpe = None
            ),
            stats = Nil
          )
        )
      )

    EnumDef(
      q"sealed trait ${Type.Name(name)}",
      q"""object ${Term.Name(name)} {
          ..$objs
      }
      """
    )
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
      case schema =>
        schema.enum match {
          case list if list.nonEmpty =>
            List(Enum(path, list))
          case Nil => Nil
        }
    }

}
