package io.github.ghostbuster91.sttp.client3.openapi.zz

import io.circe.yaml.{parser => Cparser}
import utest._
import cats.implicits._
import io.circe._
import OpenapiModels._
import OpenapiSchemaType._

object ParserTest extends TestSuite {

  val tests = Tests {
    "parse basic-structure (object) yaml" - {
      // https://swagger.io/docs/specification/basic-structure/
      val yaml = """
      |schemas:
      |  User:
      |    properties:
      |      id:
      |        type: integer
      |      name:
      |        type: string
      |    # Both properties are required
      |    required:
      |      - id
      |      - name""".stripMargin

      val res = Cparser
        .parse(yaml)
        .leftMap(err => err: Error)
        .flatMap(_.as[OpenapiComponent])

      assert(
        res ==
          Right(
            OpenapiComponent(
              Map(
                "User" -> OpenapiSchemaObject(
                  Map(
                    "id" -> OpenapiSchemaInt(false),
                    "name" -> OpenapiSchemaString(false)
                  ),
                  Seq("id", "name"),
                  false
                )
              )
            )
          )
      )
    }
    "parse basic-response (array) yaml" - {
      // https://swagger.io/docs/specification/basic-structure/
      val yaml = """application/json:
                 |  schema:
                 |    type: array
                 |    items:
                 |      type: string
                 |      """.stripMargin

      val res = Cparser
        .parse(yaml)
        .leftMap(err => err: Error)
        .flatMap(_.as[Seq[OpenapiResponseContent]])

      assert(
        res ==
          Right(
            Seq(
              OpenapiResponseContent(
                "application/json",
                OpenapiSchemaArray(OpenapiSchemaString(false), false)
              )
            )
          )
      )
    }
  }
  private def load(fileName: String): String =
    FileLoader.loadFile("circe", fileName)
}
