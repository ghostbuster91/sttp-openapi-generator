package io.github.ghostbuster91.sttp.client3.json

import scala.meta.Type

trait JsonTypeProvider {
  def anyType: Type.Name
}
