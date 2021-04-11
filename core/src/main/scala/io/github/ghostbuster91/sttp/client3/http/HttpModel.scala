package io.github.ghostbuster91.sttp.client3.http

sealed trait Method
object Method {
  case object Put extends Method
  case object Get extends Method
  case object Post extends Method
  case object Delete extends Method
  case object Head extends Method
  case object Patch extends Method
  //TODO trace, connect, option?
}

sealed abstract class MediaType(val v: String)
object MediaType {
  case object ApplicationJson extends MediaType("application/json")
  case object FormUrlEncoded
      extends MediaType("application/x-www-form-urlencoded")
  case object ApplicationOctetStream
      extends MediaType("application/octet-stream")
}
