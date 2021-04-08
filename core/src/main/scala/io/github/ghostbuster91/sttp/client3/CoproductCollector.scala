package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.circe.Coproduct
import io.github.ghostbuster91.sttp.client3.circe.Discriminator
import io.github.ghostbuster91.sttp.client3.circe.CoproductChild

class CoproductCollector(model: ModelGenerator, enums: List[Enum]) {
  def collect(
      schemas: Map[String, SafeSchema]
  ): List[Coproduct] =
    schemas.collect { case (k, c: SafeComposedSchema) =>
      collectSingle(k, c)
    }.toList

  private def collectSingle(
      key: String,
      schema: SafeComposedSchema
  ): Coproduct =
    schema.discriminator
      .map { dsc =>
        val childRef = schema.oneOf.head.ref
        val child =
          model
            .schemaFor(childRef)
            .asInstanceOf[SafeObjectSchema] //TODO handle error
        val discriminatorSchema = child.properties(dsc.propertyName)

        val discriminator = discriminatorSchema match {
          case _: SafeStringSchema =>
            Discriminator.StringDsc(
              dsc.propertyName,
              dsc.mapping
                .mapValues(ref => CoproductChild(model.classNameFor(ref)))
                .toMap
            )
          case _: SafeIntegerSchema =>
            Discriminator.IntDsc(
              dsc.propertyName,
              dsc.mapping.map { case (k, v) =>
                Integer.parseInt(k) -> CoproductChild(model.classNameFor(v))
              }.toMap
            )
          case sr: SafeRefSchema =>
            val enumClassName = model.classNameFor(sr.ref)
            val enum = enums.find(e => e.name == enumClassName).get
            val enumMap: Map[EnumValue, CoproductChild] = enum match {
              case _: Enum.IntEnum =>
                dsc.mapping.map { case (k, v) =>
                  EnumValue.IntEv(Integer.parseInt(k)) -> CoproductChild(
                    model.classNameFor(v)
                  )
                }
              case _: Enum.StringEnum =>
                dsc.mapping.map { case (k, v) =>
                  EnumValue.StringEv(k) -> CoproductChild(
                    model.classNameFor(v)
                  )
                }
            }
            Discriminator.EnumDsc(
              dsc.propertyName,
              enum,
              enumMap
            )
        }
        Coproduct(key, Some(discriminator))
      }
      .getOrElse(Coproduct(key, None))

}
