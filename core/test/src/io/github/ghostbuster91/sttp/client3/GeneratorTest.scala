package io.github.ghostbuster91.sttp.client3

import utest._
import scala.io.Source

object GeneratorTest extends TestSuite {
  val tests = Tests {
    "qwe" - {
      val yaml = load("simple_get_pet.yaml")
      val result = Generator.generateUnsafe(yaml)
      val expected = load("simple_get_pet_expected.scala2")
      assert(result == expected)
    }
  }

  private def load(fileName: String): String =
    Source
      .fromInputStream(getClass.getResourceAsStream(s"/$fileName"))
      .getLines()
      .mkString("\n")

}
