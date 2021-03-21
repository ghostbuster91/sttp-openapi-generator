package io.github.ghostbuster91.sttp.client3

import utest._
import scala.meta._
import scala.tools.reflect.ToolBox

object GeneratorTest extends TestSuite {
  val tests = Tests {
    "should parse path with GET method and object response" - {
      val yaml = load("simple_get_person.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = load("simple_get_person_expected.scala")
      assert(result.structure == expected.parse[Source].get.structure)
      expected.shouldCompile()
    }

    "should parse path with PUT method and object response" - {
      val yaml = load("simple_put_person.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = load("simple_put_person_expected.scala")
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
