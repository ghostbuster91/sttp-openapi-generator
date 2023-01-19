import mill._
import mill.define._
import mill.scalalib._

import $exec.plugins
import io.github.ghostbuster91.sttp.client3.OpenApiCodegenScalaModule

object app extends OpenApiCodegenScalaModule {
  def scalaVersion = "2.13.2"
  def mainClass = Some("com.example.Main")
}

def verify(): Command[Unit] = T.command {
  app.compile()
  ()
}
