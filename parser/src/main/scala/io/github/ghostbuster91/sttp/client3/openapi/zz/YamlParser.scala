package io.github.ghostbuster91.sttp.client3.openapi.zz

import OpenapiModels._
import io.circe.yaml.parser

object YamlParser {
  import cats.implicits._
  import io.circe._

  def parseFile(yamlString: String): Either[Error, OpenapiDocument] =
    parser
      .parse(yamlString)
      .leftMap(err => err: Error)
      .flatMap(_.as[OpenapiDocument])
}
