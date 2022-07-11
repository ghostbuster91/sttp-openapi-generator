package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.SttpOpenApiCodegenPlugin.{JsonLibrary as PluginJsonLibrary, TypesMapping as PluginTypesMapping}
import io.github.ghostbuster91.sttp.client3.{JsonLibrary as CoreJsonLibrary, TypesMapping as CoreTypesMapping}

package object implicits {
  implicit class JsonLibraryHelper(value: PluginJsonLibrary) {
    def convert: CoreJsonLibrary = value match {
      case PluginJsonLibrary.Circe => CoreJsonLibrary.Circe
    }
  }

  implicit class TypesMappingHelper(value: PluginTypesMapping) {
    def convert: CoreTypesMapping =
      CoreTypesMapping(dateTime = value.dateTime)
  }
}
