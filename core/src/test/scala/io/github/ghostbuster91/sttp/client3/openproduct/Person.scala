package io.github.ghostbuster91.sttp.client3.openproduct

case class Person[T](
    name: String,
    age: Int,
    _additionalProperties: Map[String, T]
)
