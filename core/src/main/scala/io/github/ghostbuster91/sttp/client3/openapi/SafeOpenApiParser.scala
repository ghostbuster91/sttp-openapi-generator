package io.github.ghostbuster91.sttp.client3.openapi

import io.github.ghostbuster91.sttp.client3.LogAdapter
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions

import scala.collection.JavaConverters._
import io.swagger.v3.parser.core.models.AuthorizationValue

class SafeOpenApiParser(
    log: LogAdapter,
    extensions: List[SafeOpenApi => SafeOpenApi]
) {
  def parse(yaml: String): Either[String, SafeOpenApi] = {
    val parser = new OpenAPIParser
    val opts = new ParseOptions()
    opts.setResolve(true)
    opts.setFlatten(true)
    val parserResult = parser.readContents(
      yaml,
      List.empty[AuthorizationValue].asJava,
      opts
    )
    Option(parserResult.getMessages).foreach { messages =>
      messages.asScala.foreach(log.warn)
    }
    Option(parserResult.getOpenAPI) match {
      case Some(spec) =>
        Right(
          extensions.foldLeft(new SafeOpenApi(spec))((acc, item) => item(acc))
        )
      case None =>
        Left(s"Failed to parse open api specification")
    }
  }
}
