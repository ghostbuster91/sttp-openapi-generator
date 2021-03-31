package io.github.ghostbuster91.sttp.client3

import utest._
import scala.meta._
import scala.tools.reflect.ToolBox

object GeneratorTest extends TestSuite {
  val tests = Tests {
    "simple_get_person" - test()
    "simple_put_person" - test()
    "simple_post_person" - test()
    "simple_get_nested_products" - test()
    "simple_get_person_optional" - test()
    "simple_get_product_array" - test()
    "get_with_query_param" - test()
    "get_with_multiple_query_param" - test()
    "get_with_path_param" - test()
    "get_with_multiple_path_param" - test()
    "mixed_path_and_query" - test()

    //TODO:
    //reflective compilation has failed:
    //Internal error: unable to find the outer accessor symbol of class Api
    //    scala.tools.reflect.ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal.throwIfErrors(ToolBoxFactory.scala:332)
    "string_enum" - testNoCompile()
    "int_enum" - testNoCompile()

    "get_inline_response_200" - test()
    "request_body_direct" - test()
    "request_body_indirect" - test()
    "operation_with_tag" - test()
    "multiple_operations_with_same_tag" - test()
    "multiple_operations_with_different_tag" - test()

    "coproduct" - {
      "simple" - test()
      "discriminator" - test()
      "optional_discriminator" - test()
    }
  }

  def testNoCompile()(implicit testPath: utest.framework.TestPath) = {
    val testName = testPath.value.mkString("/")
    val yaml = load(s"$testName.yaml")
    val result = Codegen.generateUnsafe(yaml)
    val expected = load(s"$testName.scala")
    assert(result.structure == expected.parse[Source].get.structure)
  }

  def test()(implicit testPath: utest.framework.TestPath) = {
    testNoCompile()
    val testName = testPath.value.mkString("/")
    val expected = load(s"$testName.scala")
    expected.shouldCompile()
  }

  private def load(fileName: String): String =
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(s"/api/$fileName"))
      .getLines()
      .mkString("\n")

  def mkToolbox(
      compileOptions: String = "",
  ): ToolBox[_ <: scala.reflect.api.Universe] = {
    val m = scala.reflect.runtime.currentMirror
    m.mkToolBox(options = compileOptions)
  }

  def compile(code: String): Unit = {
    val tb = mkToolbox()
    val tree = tb.parse(code)
    tb.compile(tree)
    ()
  }

  def compileWithoutHeader(code: String): Unit =
    compile(
      code.linesIterator.filter(!_.trim.startsWith("package")).mkString("\n"),
    )

  implicit class StringShouldCompileHelper(code: String) {
    def shouldCompile(): Unit = compileWithoutHeader(code)
  }
}
