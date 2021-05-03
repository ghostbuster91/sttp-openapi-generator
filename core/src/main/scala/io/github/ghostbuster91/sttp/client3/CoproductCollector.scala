package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import io.github.ghostbuster91.sttp.client3.openapi._

class CoproductCollector(model: Model, enums: List[Enum]) {
  def collect(
      schemas: Map[String, SafeSchema]
  ): List[Coproduct] =
    schemas
      .collect { case (k, c: SafeComposedSchema) =>
        collectOneOf(k, c) ++ collectAllOf(c)
      }
      .flatten
      .toList

  private def collectOneOf(
      key: String,
      schema: SafeComposedSchema
  ): Option[Coproduct] =
    schema.oneOf.headOption
      .map(childRef => oneOfCoproduct(key, schema, childRef))

  private def oneOfCoproduct(
      key: String,
      schema: SafeComposedSchema,
      childRef: SafeRefSchema
  ) =
    Coproduct(
      model.classNameFor(SchemaRef.schema(key)),
      schema.discriminator
        .map { dsc =>
          val child =
            model
              .schemaFor(childRef.ref)
              .asInstanceOf[SafeObjectSchema]
          val discriminatorSchema = child.properties(dsc.propertyName)
          coproductDiscriminator(dsc, discriminatorSchema)
        }
    )

  private def collectAllOf(schema: SafeComposedSchema) =
    schema.allOf.collect { case parent: SafeRefSchema =>
      val parentSchema = model
        .schemaFor(parent.ref)
        .asInstanceOf[SchemaWithProperties]
      allOfCoproduct(parent, parentSchema)
    }

  private def allOfCoproduct(
      parent: SafeRefSchema,
      parentSchema: SchemaWithProperties
  ) =
    Coproduct(
      model.classNameFor(parent.ref),
      parentSchema.discriminator
        .map { dsc =>
          val discriminatorSchema = parentSchema.properties(dsc.propertyName)
          coproductDiscriminator(dsc, discriminatorSchema)
        }
    )

  private def coproductDiscriminator(
      dsc: SafeDiscriminator,
      discriminatorSchema: SafeSchema
  ): Discriminator[_] =
    discriminatorSchema match {
      case _: SafeStringSchema =>
        Discriminator.StringDsc(
          dsc.propertyName,
          dsc.mapping
            .mapValues(ref => model.classNameFor(ref))
        )
      case _: SafeIntegerSchema =>
        Discriminator.IntDsc(
          dsc.propertyName,
          dsc.mapping.map { case (k, v) =>
            Integer.parseInt(k) -> model.classNameFor(v)
          }
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

}
