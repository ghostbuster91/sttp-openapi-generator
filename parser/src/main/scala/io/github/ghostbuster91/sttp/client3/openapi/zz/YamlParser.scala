package io.github.ghostbuster91.sttp.client3.openapi.zz

import io.circe.yaml.{parser => circeParser}
import OpenapiModels._

object YamlParser {
  import cats.implicits._
  import io.circe._

  def parseFile(yamlString: String): Either[Error, OpenapiDocument] =
    circeParser
      .parse(yamlString)
      .leftMap(err => err: Error)
      .flatMap(_.as[OpenapiDocument])
}
