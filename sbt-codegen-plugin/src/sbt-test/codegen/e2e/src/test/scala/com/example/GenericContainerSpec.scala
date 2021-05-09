package com.example

import com.dimafeng.testcontainers.{ForAllTestContainer, GenericContainer}
import com.softwaremill.diffx.scalatest.DiffMatcher
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.wait.strategy.Wait
import sttp.client3.{HttpURLConnectionBackend, Identity, SttpBackend}
import sttp.model.StatusCode

import java.io.File

class GenericContainerSpec
    extends AnyFlatSpec
    with ForAllTestContainer
    with EitherValues
    with Matchers
    with DiffInstances
    with DiffMatcher {
  override val container: GenericContainer = GenericContainer(
    "softwaremill/sttp.livestub:0.1.15",
    exposedPorts = Seq(7070),
    waitStrategy = Wait.forHttp("/__routes"),
    command = Seq("--openapi-spec", "/openapi.yaml"),
    classpathResourceMapping =
      Seq(("com/example/openapi.yaml", "openapi.yaml", BindMode.READ_ONLY))
  )

  lazy val petApi = new PetApi(
    s"http://${container.containerIpAddress}:${container.mappedPort(7070)}"
  )

  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

  it should "getPetById" in {
    val response = petApi.getPetById(1).send(backend)
    val pet = response.body.right.value
    pet should matchTo(
      Pet(
        name = "doggie",
        tags = List(
          Tag(None, Some("tag1"))
        ),
        photoUrls = List(
          "http://random-photo-url.com"
        ),
        None,
        Some(Status.Available),
        Some(Category(None, Some("Dogs")))
      )
    )
  }

  it should "findPetsByTags" in {
    val response = petApi.findPetsByTags(List("tag1")).send(backend)
    val pet = response.body.right.value
    pet should matchTo(
      List(
        Pet(
          name = "doggie",
          tags = List(
            Tag(None, Some("tag1"))
          ),
          photoUrls = List(
            "http://random-photo-url.com"
          ),
          None,
          Some(Status.Available),
          Some(Category(None, Some("Dogs")))
        )
      )
    )
  }

  it should "addPet" in {
    val pet = Pet(
      name = "doggie",
      tags = List(
        Tag(None, Some("tag1"))
      ),
      photoUrls = List(
        "http://random-photo-url.com"
      ),
      None,
      Some(Status.Available),
      Some(Category(None, Some("Dogs")))
    )
    val response = petApi.addPet(pet).send(backend)
    val petResponse = response.body.right.value
    petResponse should matchTo(pet)
  }

  it should "updatePet" in {
    val pet = Pet(
      name = "doggie",
      tags = List(
        Tag(None, Some("tag1"))
      ),
      photoUrls = List(
        "http://random-photo-url.com"
      ),
      None,
      Some(Status.Available),
      Some(Category(None, Some("Dogs")))
    )
    val response = petApi.updatePet(pet).send(backend)
    val petResponse = response.body.right.value
    petResponse should matchTo(pet)
  }

  it should "updatePetWithForm" in {
    val someString = petApi.updatePetWithForm(11L, Some("doggo"), Some("qwe"))
    val response = someString.send(backend)

    response.code shouldBe StatusCode.MethodNotAllowed
  }
  it should "findPetByStatus" in {
    val response =
      petApi.findPetsByStatus(Some(Status3.Available)).send(backend)
    val pet = response.body.right.value
    pet should matchTo(
      List(
        Pet(
          name = "doggie",
          tags = List(
            Tag(None, Some("tag1"))
          ),
          photoUrls = List(
            "http://random-photo-url.com"
          ),
          None,
          Some(Status.Available),
          Some(Category(None, Some("Dogs")))
        )
      )
    )
  }
  it should "uploadFile" in {
    val response = petApi
      .uploadFile(
        11L,
        Some("xyz"),
        Some(
          new File(
            getClass.getClassLoader
              .getResource("com/example/openapi.yaml")
              .toURI
          )
        )
      )
      .send(backend)
    response.body.right.value
  }
}
