package io.github.ghostbuster91.sttp.client3.openapi

import io.circe.yaml.parser
import io.github.ghostbuster91.sttp.client3.FileLoader
import io.swagger.v3.core.util.Yaml
import utest._

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
    val actual =
      Yaml.pretty(
        new SafeOpenApiParser(
          List(OpenApiEnumFlattener.flatten)
        ).parse(input).right.get.unsafe
      )
    assert(parser.parse(actual) == parser.parse(expected))
  }

  private def load(fileName: String): String =
    FileLoader.loadFile("openapi/enum_flattener", fileName)

}
