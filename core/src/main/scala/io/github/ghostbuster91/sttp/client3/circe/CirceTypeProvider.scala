package io.github.ghostbuster91.sttp.client3.circe

import io.github.ghostbuster91.sttp.client3.JsonTypeProvider
import io.github.ghostbuster91.sttp.client3.ImportRegistry

import scala.meta._

class CirceTypeProvider(ir: ImportRegistry) extends JsonTypeProvider {
  def anyType: Type.Name = {
    ir.registerImport(q"import _root_.io.circe.Json")
    t"Json"
  }
}
