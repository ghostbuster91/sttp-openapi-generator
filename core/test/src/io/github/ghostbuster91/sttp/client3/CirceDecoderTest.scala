package io.github.ghostbuster91.sttp.client3

import utest._
import scala.meta._

object CirceDecoderTest extends TestSuite {
  val tests = Tests {
    "string_enum" - {
      val expected = load(s"string_enum.scala").parse[Source].get
      val actual = CirceCodecGeneration
        .generate(
          List(
            Enum(
              List("Person", "Status"),
              List(EnumValue("happy"), EnumValue("neutral")),
              EnumType.EString
            )
          )
        )
        .parse[Source]
        .get
      assert(
        actual.structure == expected.structure
      )
    }
  }

  private def load(fileName: String): String =
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(s"/circe/$fileName"))
      .getLines()
      .mkString("\n")

}
