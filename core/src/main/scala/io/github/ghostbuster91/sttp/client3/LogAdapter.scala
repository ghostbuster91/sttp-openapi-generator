package io.github.ghostbuster91.sttp.client3

trait LogAdapter {
  def warn(msg: String): Unit
}

object LogAdapter {
  val StdOut: LogAdapter = new LogAdapter {
    override def warn(msg: String): Unit =
      println(s"Warning: $msg")
  }
}
