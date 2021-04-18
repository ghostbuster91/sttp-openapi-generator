package io.github.ghostbuster91.sttp.client3

import scala.meta.Import
import scala.collection.immutable.ListSet

class ImportRegistry {
  private var imports = ListSet[StringBasedCompareImport]()

  def registerImport(imp: Import): Unit =
    imports = imports + new StringBasedCompareImport(imp)

  def getImports: List[Import] = imports.toList.map(_.`import`)
}

private class StringBasedCompareImport(val `import`: Import) {
  override def hashCode(): Int = `import`.toString.hashCode

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[StringBasedCompareImport] && obj
      .asInstanceOf[StringBasedCompareImport]
      .`import`
      .toString == `import`
      .toString()
}
