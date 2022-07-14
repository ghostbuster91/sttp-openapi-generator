package io.github.ghostbuster91.sttp.client3

trait LogAdapter {
  def warn(msg: String): Unit
}

object LogAdapter {
  val StdOut: LogAdapter = (msg: String) => println(s"Warning: $msg")
}
