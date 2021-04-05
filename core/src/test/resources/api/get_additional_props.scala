package io.github.ghostbuster91.sttp.client3.example

import _root_.sttp.client3._
import _root_.sttp.model._
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi

case class Person(name: String, age: Int) extends Map[String, Any] {
  private val _internal_map = Map.empty[String, Any]

  override def updated[V1 >: Any](key: String, value: V1): Map[String, V1] =
    _internal_map.updated(key, value)

  override def get(key: String): Option[Any] = _internal_map.get(key)

  override def iterator: Iterator[(String, Any)] = _internal_map.iterator

  override def +[V1 >: Any](kv: (String, V1)): Map[String, V1] =
    _internal_map + kv
  override def -(key: String): Map[String, Any] = _internal_map - key
}

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(): Request[Person, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(asJson[Person].getRight)
}
