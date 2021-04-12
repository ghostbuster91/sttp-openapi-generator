package io.github.ghostbuster91.sttp.client3

import utest._
import io.swagger.v3.core.util.Yaml
import io.circe.yaml.parser
import io.github.ghostbuster91.sttp.client3.openapi.OpenApiLoader

object FlattenerTest extends TestSuite {
  val tests = Tests {
    "component_schemas_unnest" - test()
    "component_req_body_unnest" - test()
    "flattened_enum_left_untouched" - test()
    "header" - test()
    "path" - test()
    "query" - test()
    "request_body" - test()
    "response_body" - test()
  }

  def test()(implicit testPath: utest.framework.TestPath) = {
    val expected = load(s"${testPath.value.mkString("/")}_expected.yaml")
    val input = load(s"${testPath.value.mkString("/")}.yaml")
    val actual = Yaml.pretty(OpenApiLoader.load(input).unsafe)
    assert(parser.parse(actual) == parser.parse(expected))
  }

  private def load(fileName: String): String =
    FileLoader.loadFile("enum_flattener", fileName)

}
