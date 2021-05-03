package io.github.ghostbuster91.sttp.client3.json

import scala.meta.Type
import io.github.ghostbuster91.sttp.client3.ImportRegistry._

trait JsonTypeProvider {
  val AnyType: IM[Type.Name]
  val MapType: IM[Type.Name]
  val ErrorType: IM[Type.Name]
}
