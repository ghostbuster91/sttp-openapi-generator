package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import scala.meta._

object EnumGenerator {
  def enumToSealedTraitDef(enum: Enum) = {
    val objs = enum.values.map { value =>
      val paranetInit = init"${enum.name.typeName}()"
      q"case object ${value.simpleName} extends $paranetInit{}"
    }
    source"""sealed trait ${enum.name.typeName}
    object ${enum.name.term} {
          ..$objs
      }
    """.stats
  }

}
