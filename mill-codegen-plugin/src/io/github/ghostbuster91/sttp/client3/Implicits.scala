package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.OpenApiCodegenScalaModule.{
  FileOpts,
  JsonLibrary as PluginJsonLibrary,
  TypesMapping as PluginTypesMapping
}
import io.github.ghostbuster91.sttp.client3.{
  JsonLibrary as CoreJsonLibrary,
  TypesMapping as CoreTypesMapping
}
import os.RelPath
import upickle.default._
import mill.api.JsonFormatters

object Implicits extends JsonFormatters {

  implicit class JsonLibraryHelper(value: PluginJsonLibrary) {
    def convert: CoreJsonLibrary = value match {
      case PluginJsonLibrary.Circe => CoreJsonLibrary.Circe
    }
  }

  implicit class TypesMappingHelper(value: PluginTypesMapping) {
    def convert: CoreTypesMapping =
      CoreTypesMapping(dateTime = value.dateTime)
  }

  implicit val relPathRw: ReadWriter[RelPath] =
    implicitly[ReadWriter[IndexedSeq[String]]]
      .bimap[RelPath](
        v => v.segments,
        g => RelPath(g, 0)
      )

  implicit val typesMappingRw: ReadWriter[PluginTypesMapping] =
    implicitly[ReadWriter[Map[String, String]]].bimap[PluginTypesMapping](
      v => Map("dateTime" -> v.dateTime.getName),
      g => PluginTypesMapping(dateTime = Class.forName(g("dateTime")))
    )

  implicit val jsonLibraryCirceRw: ReadWriter[PluginJsonLibrary.Circe.type] =
    macroRW
  implicit val jsonLibraryRw: ReadWriter[PluginJsonLibrary] =
    ReadWriter.merge(jsonLibraryCirceRw)

  implicit val fileOpts: ReadWriter[FileOpts] = macroRW

  implicit val singleFileRw
  : ReadWriter[OpenApiCodegenScalaModule.Input.SingleFile] = macroRW
  implicit val directoryRw
  : ReadWriter[OpenApiCodegenScalaModule.Input.Directory] = macroRW

  implicit val inputRw: ReadWriter[OpenApiCodegenScalaModule.Input] =
    ReadWriter.merge(
      singleFileRw,
      directoryRw
    )
}

