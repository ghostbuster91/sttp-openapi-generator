package io.github.ghostbuster91.sttp.client3

class SbtLogAdapter(logger: sbt.Logger) extends LogAdapter {
  override def warn(msg: String): Unit =
    logger.warn(s"[SttpOpenapi] $msg")
}
