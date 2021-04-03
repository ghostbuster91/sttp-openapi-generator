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
          model.schemaFor(childRef).asInstanceOf[SafeObjectSchema] //TODO
        val discriminatorSchema = child.properties(dsc.propertyName)

        val discriminator = discriminatorSchema match {
          case _: SafeStringSchema =>
            Discriminator.StringDsc(
              dsc.propertyName,
              dsc.mapping.view
                .mapValues(ref => CoproductChild(model.classNameFor(ref)))
                .toMap
            )
          case _: SafeStringSchema =>
            Discriminator.IntDsc(
              dsc.propertyName,
              dsc.mapping.map { case (k, v) =>
                Integer.parseInt(k) -> CoproductChild(model.classNameFor(v))
              }.toMap
            )
          case sr: SafeRefSchema =>
            val enumClassName = model.classNameFor(sr.ref)
            val enum = enums.find(e => e.name == enumClassName).get
            Discriminator.EnumDsc(dsc.propertyName, enum, Map.empty)

        }
        Coproduct(key, Some(discriminator))
      }
      .getOrElse(Coproduct(key, None))

}
