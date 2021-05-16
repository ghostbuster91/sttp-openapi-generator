package io.github.ghostbuster91.sttp.client3

import utest._
import scala.meta._
import scala.tools.reflect.ToolBox

object GeneratorTest extends TestSuite {

  private val tb = mkToolbox()

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
      "string_enum" - test()
      "int_enum" - test()
      "component_enum" - test()
      "duplicated_enum_usage" - test()
      "duplicated_enum" - test()
    }

    "request_body" - {
      "string" - test()
      "string_with_default" - test()
      "optional_string_with_default" - test()
      "int" - test()
      "int_with_default" - test()
      "bool" - test()
      "bool_with_default" - test()
      "product" - test()
      "product_form_encoded" - test()
      "optional_product" - test()
      "array_product" - test()
      "request_body_direct" - test()
      "request_body_indirect" - test()
      "binary" - {
        "optional_file" - test()
      }
    }

    "tag" - {
      "operation_with_tag" - test()
      "multiple_operations_with_same_tag" - test()
      "multiple_operations_with_different_tag" - test()
    }

    "coproduct" - {
      "all_of" - {
        "simple" - test()
        "multiple_parents" - test()
        "multiple_siblings" - test()
      }
      "one_of" - {
        "simple" - test()
        "multiple_parents" - test()
        "string_discriminator" - test()
        "int_discriminator" - test()
        "discriminator_with_mapping" - test()
        // Following case is actually invalid because there is no way to create discriminator mapping using empty value as a key
        //"optional_discriminator" - test()
        "discriminator_enum" - test()
        "discriminator_with_enum_mapping" - test()
      }
    }

    "get_inline_response_200" - test()

    "open_product" - {
      "free_form" - test()
      "int_values" - test()
      "all_of" - test()
      "no_properties_inline" - test()
    }

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
    "errors" - {
      "product" - test(handleErrors = true)
      "multiple_errors" - test(handleErrors = true)
      "multiple_errors_with_parent" - test(handleErrors = true)
    }
    "multiple_success" - {
      "separate_products" - test()
      "common_ancestor" - test()
      "common_ancestor_semi" - test()
    }
  }

  def testNoCompile(
      handleErrors: Boolean = false
  )(implicit testPath: utest.framework.TestPath) = {
    val testName = testPath.value.mkString("/")
    val yaml = load(s"$testName.yaml")
    val result = new Codegen(
      LogAdapter.StdOut,
      CodegenConfig(
        handleErrors,
        JsonLibrary.Circe
      )
    )
      .generateUnsafe(
        yaml,
        Some("io.github.ghostbuster91.sttp.client3.example")
      )
    val expected = load(s"$testName.scala")
    assert(result.structure == expected.parse[Source].get.structure)
  }

  def test(
      handleErrors: Boolean = false
  )(implicit testPath: utest.framework.TestPath) = {
    testNoCompile(handleErrors)
    val testName = testPath.value.mkString("/")
    val expected = load(s"$testName.scala")
    expected.shouldCompile()
  }

  private def load(fileName: String): String =
    FileLoader.loadFile("api", fileName)

  def mkToolbox(
      compileOptions: String = ""
  ): ToolBox[_ <: scala.reflect.api.Universe] = {
    val m = scala.reflect.runtime.currentMirror
    m.mkToolBox(options = compileOptions)
  }

  implicit class StringShouldCompileHelper(code: String) {
    def shouldCompile(): Unit = compileWithoutHeader(code)

    private def compile(code: String): Unit = {
      val tree = tb.parse(code)
      tb.compile(tree)
      ()
    }

    private def compileWithoutHeader(code: String): Unit =
      compile(
        code.linesIterator.filter(!_.trim.startsWith("package")).mkString("\n")
      )
  }
}
