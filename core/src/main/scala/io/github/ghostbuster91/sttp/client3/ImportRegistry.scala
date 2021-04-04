package io.github.ghostbuster91.sttp.client3

import scala.meta.Import
import scala.collection.immutable.ListSet

class ImportRegistry {
  private var imports = ListSet[Import]()

  def registerImport(imp: Import): Unit =
    imports = imports + imp

  def getImports: List[Import] = imports.toList
}
