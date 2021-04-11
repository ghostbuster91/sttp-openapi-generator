package io.github.ghostbuster91.sttp.client3

package object model {
  private[model] def uncapitalized(v: String): String =
    v.take(1).toLowerCase() + v.drop(1)
}
