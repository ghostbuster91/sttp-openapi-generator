package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.OpenApiCodegenScalaModule.{JsonLibrary as PluginJsonLibrary, TypesMapping as PluginTypesMapping}
import io.github.ghostbuster91.sttp.client3.{JsonLibrary as CoreJsonLibrary, TypesMapping as CoreTypesMapping}

import os.RelPath
import upickle.default.*

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

  implicit def relPathRw: ReadWriter[RelPath] =
    implicitly[ReadWriter[IndexedSeq[String]]]
      .bimap[RelPath](
        v => v.segments,
        g => RelPath(g, 0)
      )

  //  implicit def classRw: ReadWriter[Class[Any]] = implicitly[ReadWriter[String]]
  //    .bimap[Class[Any]](
  //      v => v.getName,
  //      g => Class.forName(g).asInstanceOf[Class[Any]]
  //    )

  implicit val typesMappingRw: ReadWriter[PluginTypesMapping] =
    implicitly[ReadWriter[Map[String, String]]].bimap[PluginTypesMapping](
      v => Map("dateTime" -> v.dateTime.getName),
      g => PluginTypesMapping(dateTime = Class.forName(g("dateTime")))
    )

  implicit val jsonLibraryCirceRw: ReadWriter[PluginJsonLibrary.Circe.type] = macroRW
  implicit val jsonLibraryRw: ReadWriter[PluginJsonLibrary] =
    ReadWriter.merge(jsonLibraryCirceRw)
}
