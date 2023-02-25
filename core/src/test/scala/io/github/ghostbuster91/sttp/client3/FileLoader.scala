package io.github.ghostbuster91.sttp.client3

object FileLoader {
  def loadFile(directory: String, fileName: String) =
    try
      scala.io.Source
        .fromInputStream(
          getClass.getResourceAsStream(s"/$directory/$fileName")
        )
        .getLines()
        .mkString("\n")
    catch {
      case ex: Throwable =>
        println(s"Error while loading file $directory/$fileName")
        throw ex
    }

}
