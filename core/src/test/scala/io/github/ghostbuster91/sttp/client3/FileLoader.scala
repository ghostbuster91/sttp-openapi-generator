package io.github.ghostbuster91.sttp.client3

object FileLoader {
  def loadFile(directory: String, fileName: String) =
    scala.io.Source
      .fromInputStream(
        getClass.getResourceAsStream(s"/$directory/$fileName")
      )
      .getLines()
      .mkString("\n")

}
