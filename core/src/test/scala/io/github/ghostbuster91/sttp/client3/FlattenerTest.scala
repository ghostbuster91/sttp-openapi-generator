package io.github.ghostbuster91.sttp.client3

import utest._
import io.swagger.v3.core.util.Yaml
import io.circe.yaml.parser
import io.github.ghostbuster91.sttp.client3.openapi.OpenApiLoader

object FlattenerTest extends TestSuite {
  val tests = Tests {
    "component-schemas-unnest" - test()
    "component-req-body-unnest" - test()
    "flattened-enum-left-untouched" - test()
    "header" - test()
    "path" - test()
    "query" - test()
    "request-body" - test()
    "response-body" - test()
  }

  def test()(implicit testPath: utest.framework.TestPath) = {
    val expected = load(s"${testPath.value.mkString("/")}-expected.yaml")
    val input = load(s"${testPath.value.mkString("/")}.yaml")
    val actual = Yaml.pretty(OpenApiLoader.load(input).unsafe)
    assert(parser.parse(actual) == parser.parse(expected))
  }

  private def load(fileName: String): String =
    scala.io.Source
      .fromInputStream(
        getClass.getResourceAsStream(s"/enum-flattener/$fileName")
      )
      .getLines()
      .mkString("\n")

}
