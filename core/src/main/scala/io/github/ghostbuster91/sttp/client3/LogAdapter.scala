package io.github.ghostbuster91.sttp.client3

trait LogAdapter {}

object LogAdapter {
  val StdOut: LogAdapter = new LogAdapter {}
}
