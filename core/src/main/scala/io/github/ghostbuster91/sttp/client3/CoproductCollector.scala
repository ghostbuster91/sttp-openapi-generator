package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._

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
                .mapValues(ref => model.classNameFor(ref))
                .toMap
            )
          case _: SafeIntegerSchema =>
            Discriminator.IntDsc(
              dsc.propertyName,
              dsc.mapping.map { case (k, v) =>
                Integer.parseInt(k) -> model.classNameFor(v)
              }.toMap
            )
          case sr: SafeRefSchema =>
            val enumClassName = model.classNameFor(sr.ref)
            val enum = enums.find(e => e.name.v == enumClassName.v).get
            val enumMap: Map[EnumValue, ClassName] = enum match {
              case _: Enum.IntEnum =>
                dsc.mapping.map { case (k, v) =>
                  EnumValue.IntEv(Integer.parseInt(k)) -> model.classNameFor(v)
                }
              case _: Enum.StringEnum =>
                dsc.mapping.map { case (k, v) =>
                  EnumValue.StringEv(k) -> model.classNameFor(v)
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
