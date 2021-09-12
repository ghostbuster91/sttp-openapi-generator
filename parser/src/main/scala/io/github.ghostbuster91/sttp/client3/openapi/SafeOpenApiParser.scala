package io.github.ghostbuster91.sttp.client3.openapi

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions

import scala.collection.JavaConverters._
import io.swagger.v3.parser.core.models.AuthorizationValue

import scala.collection.Seq

class SafeOpenApiParser(extensions: List[SafeOpenApi => SafeOpenApi]) {
  def parse(yaml: String): Either[Seq[String], SafeOpenApi] = {
    val parser = new OpenAPIParser
    val opts = new ParseOptions()
    opts.setResolve(true)
    opts.setFlatten(true)
    val parserResult = parser.readContents(
      yaml,
      List.empty[AuthorizationValue].asJava,
      opts
    )
    Option(parserResult.getOpenAPI) match {
      case Some(spec) =>
        Right(
          extensions.foldLeft(new SafeOpenApi(spec))((acc, item) => item(acc))
        )
      case None =>
        Left(
          Option(parserResult.getMessages).map(_.asScala).getOrElse(Seq.empty)
        )
    }
  }
}
