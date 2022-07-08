package io.github.ghostbuster91.sttp.client3.openproduct

import io.circe.Decoder.Result
import io.circe._
import utest._

object CirceParsingTest extends TestSuite {
  override def tests: Tests = Tests {
    "should parse open product" - {
      val decoded = parser
        .decode[Person[Json]]("""{
              | "name": "Bob",
              | "age": 22,
              | "qwe" : 123
              |}""".stripMargin)
        .right
        .get
      val expected = Person[Json]("Bob", 22, Map("qwe" -> Json.fromInt(123)))
      assert(decoded == expected)
    }

    "should encode openproduct as json" - {
      val person = Person[Json]("Bob", 22, Map("qwe" -> Json.fromInt(123)))
      val json = personEncoder.apply(person)
      val expectedJson = parser
        .parse("""{
                                        | "name": "Bob",
                                        | "age": 22,
                                        | "qwe" : 123
                                        |}""".stripMargin)
        .right
        .get
      assert(json == expectedJson)
    }
  }

  implicit lazy val personDecoder: Decoder[Person[Json]] =
    new Decoder[Person[Json]] {
      override def apply(c: HCursor): Result[Person[Json]] =
        for {
          name <- c.downField("name").as[String]
          age <- c.downField("age").as[Int]
          props <- c.as[Map[String, Json]]
        } yield Person(
          name,
          age,
          props.filterKeys(_ != "name").filterKeys(_ != "age")
        )
    }

  implicit lazy val personEncoder: Encoder[Person[Json]] =
    new Encoder[Person[Json]] {
      override def apply(a: Person[Json]): Json =
        Encoder
          .forProduct2[Person[Json], String, Int]("name", "age")(p =>
            (p.name, p.age)
          )
          .apply(a)
          .deepMerge(
            Encoder[Map[String, Json]].apply(a._additionalProperties)
          )
    }

}
