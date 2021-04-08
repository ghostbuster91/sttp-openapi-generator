package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import scala.meta._

object EnumGenerator {
  def enumToSealedTraitDef(enum: Enum) = {
    val objs = enum.values.map { value =>
      val paranetInit = init"${Type.Name(enum.name)}()"
      q"case object ${value.simpleName} extends $paranetInit{}"
    }
    source"""sealed trait ${Type.Name(enum.name)}
    object ${Term.Name(enum.name)} {
          ..$objs
      }
    """.stats
  }

}
