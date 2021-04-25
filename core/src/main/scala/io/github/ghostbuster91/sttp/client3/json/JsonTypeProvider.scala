package io.github.ghostbuster91.sttp.client3.json

import scala.meta.Type
import io.github.ghostbuster91.sttp.client3.ImportRegistry._

trait JsonTypeProvider {
  def anyType: IM[Type.Name]
  def errorType: Type
}
