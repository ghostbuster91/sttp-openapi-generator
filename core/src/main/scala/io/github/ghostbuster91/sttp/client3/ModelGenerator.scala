package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.model._
import scala.meta._

object ModelGenerator {
  def generate(
      coproducts: List[Coproduct],
      products: List[Product]
  ): List[Defn] = {
    val classes = coproducts
      .sortBy(_.name)
      .map(schemaToSealedTrait)
    val traits = products
      .sortBy(_.name)
      .map(schemaToClassDef)
    classes ++ traits
  }

  private def schemaToClassDef(
      product: Product
  ): Defn =
    product.parents match {
      case parents if parents.nonEmpty =>
        val parentInits = parents.sortBy(_.v).map(p => init"${p.typeName}()")
        q"case class ${product.name.typeName}(..${product.allProperties.map(_.asParam)}) extends ..$parentInits"
      case Nil =>
        q"case class ${product.name.typeName}(..${product.allProperties.map(_.asParam)})"
    }

  private def schemaToSealedTrait(
      coproduct: Coproduct
  ): Defn.Trait = {
    val defParams =
      (coproduct.properties ++ coproduct.additionalProperties).map(_.asDef)
    q"""sealed trait ${coproduct.name.typeName} {
            ..$defParams
        }
        """
  }
}
