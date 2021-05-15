import _root_.io.circe.generic.AutoDerivation
import _root_.sttp.client3.circe.SttpCirceApi

trait CirceCodecs extends AutoDerivation with SttpCirceApi {
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

object CirceCodecs extends CirceCodecs
