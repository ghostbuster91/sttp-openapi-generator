import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends SttpCirceApi {
  implicit lazy val personDecoder: Decoder[Person] =
    Decoder.forProduct2("age", "name")(Person.apply)
  implicit lazy val personEncoder: Encoder[Person] =
    Encoder.forProduct2("age", "name")(p => (p.age, p.name))
}
object CirceCodecs extends CirceCodecs
