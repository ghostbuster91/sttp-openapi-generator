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
  }

  def test()(implicit testPath: utest.framework.TestPath) = {
    val testName = testPath.value.last
    val yaml = load(s"$testName.yaml")
    val result = Generator.generateUnsafe(yaml).parse[Source].get
    val expected = load(s"$testName.scala")
    assert(result.structure == expected.parse[Source].get.structure)
    expected.shouldCompile()
  }

  private def load(fileName: String): String =
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(s"/$fileName"))
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
