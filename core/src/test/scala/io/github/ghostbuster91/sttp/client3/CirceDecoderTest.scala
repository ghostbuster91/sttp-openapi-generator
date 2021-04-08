package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.circe.CirceCodecGenerator
import io.github.ghostbuster91.sttp.client3.model._
import utest._
import scala.meta._

object CirceDecoderTest extends TestSuite {
  val tests = Tests {
    "string_enum" - {
      val expected = load(s"string_enum.scala").parse[Source].get
      val actual = new CirceCodecGenerator(new ImportRegistry())
        .generate(
          List(
            Enum.StringEnum(
              ClassName("PersonStatus"),
              List(EnumValue.StringEv("happy"), EnumValue.StringEv("neutral"))
            )
          ),
          Nil,
          Nil
        )
      assert(actual.structure == expected.structure)
    }
  }

  private def load(fileName: String): String =
    scala.io.Source
      .fromInputStream(getClass.getResourceAsStream(s"/circe/$fileName"))
      .getLines()
      .mkString("\n")

}
