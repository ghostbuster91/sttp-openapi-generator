package io.github.ghostbuster91.sttp.client3

import scala.meta.Type

trait JsonTypeProvider {
  def anyType: Type.Name
}
