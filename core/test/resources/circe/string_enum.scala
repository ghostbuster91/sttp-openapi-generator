package io.github.ghostbuster91.sttp.client3

import io.circe.Decoder
import io.circe.Encoder

trait CirceCodecs {
  implicit val personStatusDecoder: Decoder[PersonStatus] =
    Decoder.decodeString.emap({
      case "happy" =>
        Right(PersonStatus.Happy)
      case "neutral" =>
        Right(PersonStatus.Neutral)
      case other =>
        Left("Unexpected value for enum:" + other)
    })
  implicit val personStatusEncoder: Encoder[PersonStatus] =
    Encoder.encodeString.contramap({
      case PersonStatus.Happy   => "happy"
      case PersonStatus.Neutral => "neutral"
    })
}
