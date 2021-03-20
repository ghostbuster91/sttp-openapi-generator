package io.github.ghostbuster91.sttp.client3

import utest._
import scala.meta._

object GeneratorTest extends TestSuite {
  val tests = Tests {
    "qwe" - {
      val yaml = load("simple_get_pet.yaml")
      val result = Generator.generateUnsafe(yaml).parse[Source].get
      val expected = loadExpected("simple_get_pet_expected.scala")
      println(result.structure)
      assert(result.structure == expected.structure)
    }
  }

  private def loadExpected(fileName: String) = load(fileName).parse[Source].get

  private def load(fileName: String): String =
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(s"/$fileName"))
      .getLines()
      .mkString("\n")
}
