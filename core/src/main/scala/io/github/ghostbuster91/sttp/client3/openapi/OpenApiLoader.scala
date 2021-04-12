package io.github.ghostbuster91.sttp.client3.openapi

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import scala.collection.JavaConverters._
import io.swagger.v3.parser.core.models.AuthorizationValue
import io.github.ghostbuster91.sttp.client3.SafeOpenApi

object OpenApiLoader {
  def load(yaml: String): SafeOpenApi = {
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
      messages.asScala.foreach(println)
    }
    Option(parserResult.getOpenAPI) match {
      case Some(spec) =>
        OpenApiEnumFlattener.flatten(spec)
      case None =>
        throw new RuntimeException(s"Failed to parse k8s swagger specs")
    }
  }
}
