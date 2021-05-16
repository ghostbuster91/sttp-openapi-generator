package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.json.circe.CirceCodecGenerator
import io.github.ghostbuster91.sttp.client3.model._
import utest._
import scala.meta._

object CirceDecoderTest extends TestSuite {
  val tests = Tests {
    "string_enum" - {
      val expected = load(s"string_enum.scala").parse[Source].get
      val actual = new CirceCodecGenerator()
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
        .run(ImportRegistry())
        .value
        ._2
      assert(actual.structure == expected.structure)
    }

    "product" - {
      val expected = load(s"product.scala").parse[Source].get
      val actual = new CirceCodecGenerator()
        .generate(
          Nil,
          Nil,
          List(
            Product.Regular(
              ClassName("Person"),
              List.empty,
              List(
                ParameterRef(t"Int", ParameterName("age"), None),
                ParameterRef(t"String", ParameterName("name"), None)
              )
            )
          )
        )
        .run(ImportRegistry())
        .value
        ._2
      assert(actual.structure == expected.structure)
    }
  }

  private def load(fileName: String): String =
    FileLoader.loadFile("circe", fileName)

}
