package io.github.ghostbuster91.sttp.client3

import utest._
import scala.meta._
import scala.tools.reflect.ToolBox

object GeneratorTest extends TestSuite {
  val tests = Tests {
    "static get with response object" - {
      val yaml = load("simple_get_person.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = load("simple_get_person.scala")
      assert(result.structure == expected.parse[Source].get.structure)
      expected.shouldCompile()
    }

    "put request & response objects " - {
      val yaml = load("simple_put_person.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = load("simple_put_person.scala")
      assert(result.structure == expected.parse[Source].get.structure)
      expected.shouldCompile()
    }

    "post request & response objects" - {
      val yaml = load("simple_post_person.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = load("simple_post_person.scala")
      assert(result.structure == expected.parse[Source].get.structure)
      expected.shouldCompile()
    }

    "static get - nested products" - {
      val yaml = load("simple_get_nested_products.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = load("simple_get_nested_products.scala")
      assert(result.structure == expected.parse[Source].get.structure)
      expected.shouldCompile()
    }

    "static get - optional values" - {
      val yaml = load("simple_get_person_optional.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = load("simple_get_person_optional.scala")
      assert(result.structure == expected.parse[Source].get.structure)
      expected.shouldCompile()
    }
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
