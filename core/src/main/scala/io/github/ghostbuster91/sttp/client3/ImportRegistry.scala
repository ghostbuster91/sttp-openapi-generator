package io.github.ghostbuster91.sttp.client3

import cats.data.State

import scala.meta.{Import, Importee, Type}
import scala.collection.immutable.ListSet

class ImportRegistry(
    imports: ListSet[StringBasedCompareImport]
) {

  def registerImport(imp: Import): ImportRegistry =
    new ImportRegistry(imports + new StringBasedCompareImport(imp))

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

object ImportRegistry {
  type IM[A] = State[ImportRegistry, A]

  def registerExternalTpe(imp: Import): IM[Type.Name] =
    imp.importers match {
      case ::(importer, Nil) =>
        importer.importees match {
          case ::(head: Importee.Name, Nil) =>
            State(prev =>
              prev.registerImport(imp) -> Type.Name(head.name.value)
            )
          case _ =>
            throw new IllegalArgumentException(
              "Multiple imports are unsupported"
            )
        }
      case _ =>
        throw new IllegalArgumentException(
          "Multiple imports are unsupported"
        )
    }

  def pure[T](v: T): IM[T] = State.pure[ImportRegistry, T](v)

  def apply(im: Import*): ImportRegistry =
    new ImportRegistry(ListSet(im.map(new StringBasedCompareImport(_)): _*))
}
