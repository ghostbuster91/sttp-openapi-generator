# sttp-openapi-generator
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fghostbuster91%2Fsttp-openapi-generator.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Fghostbuster91%2Fsttp-openapi-generator?ref=badge_shield)
[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/ghostbuster91/sttp-openapi-generator/.github/workflows/main.yml?style=for-the-badge" height="24">](https://github.com/ghostbuster91/sttp-openapi-generator/actions)
[<img alt="Maven Central" src="https://img.shields.io/maven-central/v/io.github.ghostbuster91.sttp-openapi/codegen-core_2.12?style=for-the-badge" height="24">](https://search.maven.org/artifact/io.github.ghostbuster91.sttp-openapi/codegen-core_2.12)
[<img src="https://github.com/ghostbuster91/sttp-openapi-generator/raw/master/sourcegraph-mark.svg" width="25">](https://sourcegraph.com/github.com/ghostbuster91/sttp-openapi-generator)

The generator can be used in projects with following scala versions: 2.12, 2.13 and 3.x

## Farewell

I decided to archive this project. I kept putting off the rewrite of its internals to make the parser more robust but I think that it will never happen. 
I no longer have the energy nor the ambition to keep this project alive. Also, nowadays there are better alternatives to describe a service interface (e.g. [smithy](https://smithy.io/2.0/index.html) or [protobufs](https://protobuf.dev/)). 

If you came here looking for a solution to generate scala code from some description of your api/interface I can recommend following options:
- [Guardrail](https://github.com/guardrail-dev/guardrail) if you already have openapi spec
- [smithy4s](https://github.com/disneystreaming/smithy4s) if you don't have yet openapi, and you are willing to use something better than yaml/json


## Why?

Why creating another openapi-generator when there is an official one? While the mentioned generator is generally a great project and serves well for many people its scala part has a few flaws in my opinion. There is no proper encoding for discriminators, neither support for other json libraries. The genereted code doesn't feel like native. These, as well as the other things, could (and probably will at some point) be implemented, but the size of the project and underlying templating engine(mustache) don't make it easier. Last but not least it is currently impossible to generate openapi code into src-managed directory (https://github.com/OpenAPITools/openapi-generator/issues/6685). I think that, by extracting and focusing only on a scala related part, it can be done better.

## Goals of the project

- generate idiomatic scala code
- support popular json libararies from scala ecosystem
- support only sttp but do it well
- [proper integration with sbt and other build tools](#sbt-plugin)
- [support discriminators](#discriminators)
- [support error encoding](#error-encoding)
- [support open products](#open-product)
- [comparison to similar projects](#comparison-to-similar-projects)

## Teaser

Given following yaml:

```yaml
openapi: 3.0.3
info:
  title: Entities
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
                $ref: "#/components/schemas/Person"
components:
  schemas:
    Person:
      required:
        - name
        - age
      type: object
      properties:
        name:
          type: string
        age:
          type: integer
          minimum: 11
```

it will be turned into:

```scala
trait CirceCodecs extends SttpCirceApi {
  implicit lazy val personDecoder: Decoder[Person] =
    Decoder.forProduct2("name", "age")(Person.apply)
  implicit lazy val personEncoder: Encoder[Person] =
    Encoder.forProduct2("name", "age")(p => (p.name, p.age))
}
object CirceCodecs extends CirceCodecs

case class Person(name: String, age: Int)

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._

  def getRoot(): Request[Person, Any] = basicRequest
    .get(uri"$baseUrl")
    .response(
      fromMetadata(
        asJson[Person].getRight,
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(200),
          asJson[Person].getRight
        )
      )
    )
}
```
## Getting started

### sbt-plugin

You can use sttp-openapi-codegen seamlessly with sbt via a dedicated sbt plugin. 
In order to use it, follow the usual convention, first add it to `project/plugins.sbt`:
```scala
addSbtPlugin("io.github.ghostbuster91.sttp-openapi" % "sbt-codegen-plugin" % <version>)
```

next, enable it for the desired modules in `build.sbt`:
```scala
import SttpOpenApiCodegenPlugin._
enablePlugins(SttpOpenApiCodegenPlugin)
```

Generator will walk through all files in input directory and generate for each one respective code into output directory.
Package name based on directory structure will be preserved.

Code generation can be configured by one of the following options:

- `sttpOpenApiOutputPath` - Directory for sources generated by sttp-openapi generator (default: `target/scala-2.12/src_managed/`)

- `sttpOpenApiInputPath` - Input resources for sttp-openapi generator (default: `./resources`)

- `sttpOpenApiJsonLibrary` - Json library for sttp-openapi generator to use (currently only `Circe`)

- `sttpOpenApiHandleErrors` - If true the generator will include error information in types (default: `true`)

For always up-to-date examples of codegen plugin usage refer to sbt tests: https://github.com/ghostbuster91/sttp-openapi-generator/tree/master/sbt-codegen-plugin/src/sbt-test/codegen/simple

### mill-plugin

You can use sttp-openapi-codegen seamlessly with mill via a dedicated mill plugin. 
```scala
import mill._, mill.scalalib._

import $ivy.`io.github.ghostbuster91.sttp-openapi::mill-codegen-plugin:<version>`
import io.github.ghostbuster91.sttp.client3.OpenApiCodegenScalaModule

object app extends OpenApiCodegenScalaModule {
  def scalaVersion = "2.13.2"
  def mainClass = Some("com.example.Main")
}
```

Additional configuration options:
```scala
/** Input resources for sttp-openapi generator
  */
def sttpOpenApiInput: T[Seq[Input]] = T {
  Seq(
    Input.dir(millSourcePath / "src" / "main" / "openapi"),
    Input.dir(millSourcePath / "openapi")
  )
}

/** Json library for sttp-openapi generator to use
  */
def sttpOpenApiJsonLibrary: T[JsonLibrary] = T(JsonLibrary.Circe: JsonLibrary)

/** If true the generator will include error information in types
  */
def sttpOpenApiHandleErrors: T[Boolean] = T(true)

/** If true the generator will render model classes only if they are
  * referenced by any of the exiting operations
  */
def sttpOpenApiMinimizeOutput: T[Boolean] = T(true)

/** Additional types mapping configuration
  */
def sttpOpenApiTypesMapping: T[TypesMapping] = T(TypesMapping())
```

For always up-to-date examples of codegen plugin usage refer to mill plugin integration tests: https://github.com/ghostbuster91/sttp-openapi-generator/tree/master/mill-codegen-plugin-itest

## discriminators

In the openapi specification there is a notion of [discriminators](https://swagger.io/docs/specification/data-models/inheritance-and-polymorphism/). 
These objects are used to distinguishing between polymorphic instances of some type based on a given value.

This project takes advantage of them and generates json configs accordingly.

```yaml
components:
  schemas:
    Entity:
      oneOf:
        - $ref: "#/components/schemas/Person"
        - $ref: "#/components/schemas/Organization"
      discriminator:
        propertyName: name
        mapping:
          john: "#/components/schemas/Person"
          sml: "#/components/schemas/Organization"
    Person:
      required:
        - name
        - age
      type: object
      properties:
        name:
          type: string
        age:
          type: integer
    Organization:
      required:
        - name
        - size
      type: object
      properties:
        name:
          type: string
        size:
          type: integer
```

```scala
sealed trait Entity
case class Organization(size: Int) extends Entity()
case class Person(name: String, age: Int) extends Entity()

trait CirceCodecs extends SttpCirceApi {
  // codecs for Person and Organization omitted for readability

  implicit lazy val entityDecoder: Decoder[Entity] = new Decoder[Entity]() {
    override def apply(c: HCursor): Result[Entity] = c
      .downField("name")
      .as[String]
      .flatMap({
        case "john" => Decoder[Person].apply(c)
        case "sml"  => Decoder[Organization].apply(c)
        case other =>
          Left(DecodingFailure("Unexpected value for coproduct:" + other, Nil))
      })
  }
  implicit lazy val entityEncoder: Encoder[Entity] = new Encoder[Entity]() {
    override def apply(entity: Entity): Json = entity match {
      case person: Person => Encoder[Person].apply(person)
      case organization: Organization =>
        Encoder[Organization].apply(organization)
    }
  }
}
```

## error encoding

In openapi error responses can be represented equally easily as success ones.
That is also the case for the sttp client. 
If you are not a fan of error handling, you can disable that feature in generator settings.

```yaml
openapi: 3.0.2
info:
  title: Entities
  version: "1.0"
paths:
  /person:
    put:
      summary: Update an existing person
      description: Update an existing person by Id
      operationId: updatePerson
      responses:
        "400":
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/ErrorModel"
        "401":
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/ErrorModel2"
components:
  schemas:
    ErrorModel:
      required:
        - msg
      type: object
      properties:
        msg:
          type: string
    ErrorModel2:
      required:
        - msg
      type: object
      properties:
        msg:
          type: string
```

```scala
sealed trait UpdatePersonGenericError
case class ErrorModel(msg: String) extends UpdatePersonGenericError()
case class ErrorModel2(msg: String) extends UpdatePersonGenericError()

class DefaultApi(baseUrl: String, circeCodecs: CirceCodecs = CirceCodecs) {
  import circeCodecs._
 
  def updatePerson(): Request[
    Either[ResponseException[UpdatePersonGenericError, CirceError], Unit],
    Any
  ] = basicRequest
    .put(uri"$baseUrl/person")
    .response(
      fromMetadata(
        asJsonEither[UpdatePersonGenericError, Unit],
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(400),
          asJsonEither[ErrorModel, Unit]
        ),
        ConditionalResponseAs(
          _.code == StatusCode.unsafeApply(401),
          asJsonEither[ErrorModel2, Unit]
        )
      )
    )
}
```

## open-product

In openapi specifications data models can be extended by [arbitrary properties if needed](https://swagger.io/docs/specification/data-models/dictionaries/). 
To do that one has to specify `additionalProperties` on particular model. At the same time on the call site special codecs need to be provided to support such types.
Luckily, sttp-openapi-generator will handle that as well.

```yaml
openapi: 3.0.3
info:
  title: Entities
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
                $ref: "#/components/schemas/Person"
components:
  schemas:
    Person:
      required:
        - name
        - age
      type: object
      properties:
        name:
          type: string
        age:
          type: integer
      additionalProperties: true
```

```scala
trait CirceCodecs extends SttpCirceApi {
  implicit lazy val personDecoder: Decoder[Person] = new Decoder[Person]() {
    override def apply(c: HCursor): Result[Person] =
      for {
        name <- c.downField("name").as[String]
        age <- c.downField("age").as[Int]
        additionalProperties <- c.as[Map[String, Json]]
      } yield Person(
        name,
        age,
        additionalProperties.filterKeys(_ != "name").filterKeys(_ != "age")
      )
  }
  implicit lazy val personEncoder: Encoder[Person] = new Encoder[Person]() {
    override def apply(person: Person): Json = Encoder
      .forProduct2[Person, String, Int]("name", "age")(p => (p.name, p.age))
      .apply(person)
      .deepMerge(
        Encoder[Map[String, Json]].apply(person._additionalProperties)
      )
  }

}

case class Person(
    name: String,
    age: Int,
    _additionalProperties: Map[String, Json]
)
```

## Comparison to similar projects

Apart from official openApi generator which was mentioned in the [Why?](#why?) section there are other similar projects.

- [Guardrail](https://github.com/guardrail-dev/guardrail)

    Guardrail can generate both client and server code. When it comes to client code generation it is similar to that project,
    although it supports http4s and akka-http, while this project focuses solely on sttp.

## Contributing

Contributions are more than welcome. This is an early stage project which means that everything is subject to change.

See the list of issues and pick one! Or report your own.

If you are having doubts on the why or how something works, don't hesitate to ask a question.


## Releasing a new version

Push a new tag to the master branch.

## License
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fghostbuster91%2Fsttp-openapi-generator.svg?type=large)](https://app.fossa.com/projects/git%2Bgithub.com%2Fghostbuster91%2Fsttp-openapi-generator?ref=badge_large)
