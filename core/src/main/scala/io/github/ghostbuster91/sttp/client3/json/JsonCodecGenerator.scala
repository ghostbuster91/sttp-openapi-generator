package io.github.ghostbuster91.sttp.client3.json

import io.github.ghostbuster91.sttp.client3.ImportRegistry.IM
import io.github.ghostbuster91.sttp.client3.model._

import scala.meta.Source

trait JsonCodecGenerator {
  def generate(
      enums: List[Enum],
      coproducts: List[Coproduct],
      openProducts: List[OpenProduct]
  ): IM[Source]
}
