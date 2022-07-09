package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.GeneratorTest.StringShouldCompileHelper
import utest._

object PetStoreTest extends TestSuite {

  val tests = Tests {
    "pet_store" - testCompile()
  }

  def testCompile()(implicit testPath: utest.framework.TestPath) = {
    val testName = testPath.value.mkString("/")
    val yaml = load(s"$testName.yaml")
    val codegen = new Codegen(
      LogAdapter.StdOut,
      CodegenConfig(
        handleErrors = true,
        JsonLibrary.Circe,
        minimize = true,
        TypesMapping()
      )
    )
    val result = codegen
      .generateUnsafe(
        yaml,
        Some("io.github.ghostbuster91.sttp.client3.example")
      )
    result.syntax.shouldCompile()
  }

  private def load(fileName: String): String =
    FileLoader.loadFile("api", fileName)
}
