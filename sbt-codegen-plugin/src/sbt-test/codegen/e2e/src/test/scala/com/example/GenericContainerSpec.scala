package com.example

import com.dimafeng.testcontainers.{ForAllTestContainer, GenericContainer}
import com.softwaremill.diffx.scalatest.DiffMatcher
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.wait.strategy.Wait
import sttp.client3.HttpURLConnectionBackend

class GenericContainerSpec
    extends AnyFlatSpec
    with ForAllTestContainer
    with EitherValues
    with Matchers
    with DiffInstances
    with DiffMatcher {
  override val container: GenericContainer = GenericContainer(
    "softwaremill/sttp.livestub:0.1.13",
    exposedPorts = Seq(7070),
    waitStrategy = Wait.forHttp("/__routes"),
    command = Seq("--openapi-spec", "/openapi.yaml"),
    classpathResourceMapping =
      Seq(("com/example/openapi.yaml", "openapi.yaml", BindMode.READ_ONLY))
  )

  it should "getPetById" in {
    val petApi = new PetApi(
      s"http://${container.containerIpAddress}:${container.mappedPort(7070)}"
    )
    val response = petApi.getPetById(1).send(HttpURLConnectionBackend())
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
}
