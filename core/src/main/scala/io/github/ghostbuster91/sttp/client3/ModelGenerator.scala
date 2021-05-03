package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.ImportRegistry._
import cats.syntax.all._
import io.github.ghostbuster91.sttp.client3.json._
import io.github.ghostbuster91.sttp.client3.openapi._
import io.github.ghostbuster91.sttp.client3.ModelGenerator._
import io.github.ghostbuster91.sttp.client3.model._
import cats.Eval
import cats.data.IndexedStateT

import scala.annotation.tailrec
import scala.meta._

class ModelGenerator(
    model: Model,
    jsonTypeProvider: JsonTypeProvider
) {
  def generate(
      coproducts: List[Coproduct],
      products: List[Product]
  ): List[Defn] =
    collectTraits(coproducts.sortBy(_.name)) ++ collectClasses(
      products.sortBy(_.name)
    )

  private def collectClasses(products: List[Product]) =
    products
      .map(schemaToClassDef)

  private def collectTraits(coproducts: List[Coproduct]) =
    coproducts.map(schemaToSealedTrait)

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

object ModelGenerator {
  def apply(
      model: Model,
      jsonTypeProvider: JsonTypeProvider
  ): ModelGenerator =
    new ModelGenerator(
      model,
      jsonTypeProvider
    )

}
