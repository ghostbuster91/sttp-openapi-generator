# sttp-openapi-generator

_This project is in a very early stage, use it at your own risk!_

## Why?

Why creating another openapi-generator when there is an official one? While the mentioned generator is generally a great project and serves well for many people its scala part has a few flaws in my opinion. There is no proper encoding for discriminators, neither support for other json libraries. The genereted code doesn't feel like native. These, as well as the other things, could (and probably will at some point) be implemented, but the size of the project and underlying templating engine(mustache) don't make it easier. Last but not least it is currently impossible to generate openapi code into src-managed directory (https://github.com/OpenAPITools/openapi-generator/issues/6685). I think that, by extracting and focusing only on a scala related part, it can be done better.

## Goals of the project

- generate code which feels like native and always compile
- support popular json libararies from scala ecosystem
- support major sttp version
- support discriminators
- proper integration with sbt and other build tools
- support for error encoding

## Teaser

Given following yaml:

```yaml
openapi: 3.0.3
info:
  title: Fruits
  version: "1.0"
paths:
  /:
    get:
      operationId: getRoot
      responses:
        "200":
          description: ""
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/Entity"
components:
  schemas:
    Entity:
      oneOf:
        - $ref: "#/components/schemas/Person"
        - $ref: "#/components/schemas/Organization"
      discriminator:
        propertyName: name
    PersonName:
      type: string
      enum:
        - bob
        - alice
    Person:
      required:
        - name
        - age
      type: object
      properties:
        name:
          $ref: "#/components/schemas/PersonName"
        age:
          type: integer
    Organization:
      required:
        - name
      type: object
      properties:
        name:
          $ref: "#/components/schemas/PersonName"
```

it will be turned into:

```scala
sealed trait PersonName
object PersonName {
  case object Bob extends PersonName()
  case object Alice extends PersonName()
}

sealed trait Entity { def name: PersonName }
case class Person(name: PersonName, age: Int) extends Entity()
case class Organization(name: PersonName) extends Entity()

class DefaultApi(baseUrl: String) extends CirceCodecs {
  def getRoot(): Request[Entity, Any] =
    basicRequest.get(uri"$baseUrl").response(asJson[Entity].getRight)
}

trait CirceCodecs extends AutoDerivation with SttpCirceApi {

  implicit val personNameDecoder: Decoder[PersonName] =
    Decoder.decodeString.emap({
      case "bob"   => Right(PersonName.Bob)
      case "alice" => Right(PersonName.Alice)
      case other   => Left("Unexpected value for enum:" + other)
    })
  implicit val personNameEncoder: Encoder[PersonName] =
    Encoder.encodeString.contramap({
      case PersonName.Bob   => "bob"
      case PersonName.Alice => "alice"
    })
  implicit val entityDecoder: Decoder[Entity] = new Decoder[Entity] {
    override def apply(c: HCursor): Result[Entity] = c
      .downField("name")
      .as[PersonName]
      .flatMap({
        case PersonName.Bob   => Decoder[Person].apply(c)
        case PersonName.Alice => Decoder[Organization].apply(c)
        case other =>
          Left(DecodingFailure("Unexpected value for coproduct:" + other, Nil))
      })
  }
  implicit val entityEncoder: Encoder[Entity] = new Encoder[Entity] {
    override def apply(entity: Entity): Json = entity match {
      case person: Person => Encoder[Person].apply(person)
      case organization: Organization =>
        Encoder[Organization].apply(organization)
    }
  }
}
```
