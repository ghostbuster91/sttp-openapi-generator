package io.github.ghostbuster91.sttp.client3

import utest._
import scala.meta._
import scala.tools.reflect.ToolBox

object GeneratorTest extends TestSuite {
  val tests = Tests {

    "simple" - {
      "simple_get_person" - test()
      "post" - test()
      "simple_get_nested_products" - test()
      "simple_get_person_optional" - test()
      "simple_get_product_array" - test()
      "simple_put_no_response" - test()
      "simple_get_reserved" - test()
    }
    "uri" - {
      "get_with_query_param" - test()
      "get_with_optional_query_param" - test()
      "get_with_multiple_query_param" - test()
      "get_with_path_param" - test()
      "get_with_path_param_fixed_ending" - test()
      "get_with_multiple_path_param" - test()
      "get_with_multiple_path_param_2" - test()
      "mixed_path_and_query" - test()
    }
    "enum" - {
      //TODO:
      //reflective compilation has failed:
      //Internal error: unable to find the outer accessor symbol of class Api
      //    scala.tools.reflect.ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal.throwIfErrors(ToolBoxFactory.scala:332)
      "string_enum" - testNoCompile()
      "int_enum" - testNoCompile()
      "component_enum" - testNoCompile()
    }

    "request_body" - {
      "simple" - test()
      "optional" - test()
      "request_body_direct" - test()
      "request_body_indirect" - test()
    }

    "tag" - {
      "operation_with_tag" - test()
      "multiple_operations_with_same_tag" - test()
      "multiple_operations_with_different_tag" - test()
    }

    "coproduct" - {
      "simple" - test()
      "string_discriminator" - test()
      "int_discriminator" - test()
      "discriminator_with_mapping" - test()
      // Following case is actually invalid because there is no way to create discriminator mapping using empty value as a key
      //"optional_discriminator" - test()
      "discriminator_enum" - testNoCompile()
    }

    "get_inline_response_200" - test()
    "get_additional_props" - test()

    "format" - {
      "int32" - test()
      "int64" - test()
      "double" - test()
      "float" - test()
      "uuid" - test()
    }

    "header" - {
      "string" - test()
      "array_of_strings" - test()
      "optional" - test()
      "int" - test()
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
      compileOptions: String = ""
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
      code.linesIterator.filter(!_.trim.startsWith("package")).mkString("\n")
    )

  implicit class StringShouldCompileHelper(code: String) {
    def shouldCompile(): Unit = compileWithoutHeader(code)
  }
}
