package io.github.ghostbuster91.sttp.client3.api

import io.github.ghostbuster91.sttp.client3.ImportRegistry.IM
import io.github.ghostbuster91.sttp.client3._
import io.github.ghostbuster91.sttp.client3.openapi._
import io.github.ghostbuster91.sttp.client3.json.JsonTypeProvider
import cats.syntax.all._
import cats.data.NonEmptyList
import sttp.model.StatusCode

import scala.meta._

private class ResponseSpecGenerator(
    config: CodegenConfig,
    model: Model,
    jsonTypeProvider: JsonTypeProvider
) {
  def apply(operation: SafeOperation): IM[ResponseSpec] = {
    val successResponseSpecs =
      operation.collectResponses(statusCode => statusCode.isSuccess)
    val errorResponseSpecs =
      operation.collectResponses(statusCode => statusCode.isClientError)
    for {
      successAncestorType <- getCommonAncestor(
        successResponseSpecs.values.toList
      ).map(_.getOrElse(t"Unit"))
      errorAncestorType <- getCommonAncestor(errorResponseSpecs.values.toList)
      successCodesWithTypes <- collectSuccessCodesWithTypes(
        successResponseSpecs
      )
      errorCodesWithTypes <- collectErrorWithTypes(errorResponseSpecs)
      returnTpe <- calculateReturnType(successAncestorType, errorAncestorType)
    } yield responseSpec(
      successAncestorType,
      errorAncestorType,
      successCodesWithTypes,
      errorCodesWithTypes,
      returnTpe
    )
  }

  private def responseSpec(
      successAncestorType: Type,
      errorAncestorType: Option[Type],
      successCodesWithTypes: Map[StatusCode, Type],
      errorCodesWithTypes: Map[StatusCode, Type],
      returnTpe: Type
  ) = {
    val asJsonWrapper =
      (errorType: Option[Type], successType: Type) =>
        errorType match {
          case Some(value) => q"asJsonEither[$value, $successType]"
          case None        => q"asJson[$successType]"
        }

    val responseAsCases = (successCodesWithTypes.mapValues(tpe =>
      flattenErrors(asJsonWrapper(errorAncestorType, tpe))
    ) ++ errorCodesWithTypes.mapValues(tpe =>
      flattenErrors(asJsonWrapper(Some(tpe), successAncestorType))
    )).map { case (statusCode, asJson) =>
      q"ConditionalResponseAs(_.code == StatusCode.unsafeApply(${statusCode.code}), $asJson)"
    }.toList

    val topAsJson = flattenErrors(
      asJsonWrapper(errorAncestorType, successAncestorType)
    )

    ResponseSpec(
      returnTpe,
      q"""fromMetadata(
                $topAsJson,
                ..$responseAsCases
                )"""
    )
  }

  private def flattenErrors = (term: Term) =>
    if (config.handleErrors) {
      term
    } else {
      q"$term.getRight"
    }

  private def calculateReturnType(
      successAncestorType: Type,
      errorAncestorType: Option[Type]
  ): IM[Type] =
    if (config.handleErrors) {
      jsonTypeProvider.ErrorType.map { deserializationErrorTpe =>
        errorAncestorType match {
          case Some(value) =>
            t"Either[ResponseException[$value, $deserializationErrorTpe], $successAncestorType]"
          case None =>
            t"Either[ResponseException[String, $deserializationErrorTpe], $successAncestorType]"
        }
      }
    } else {
      successAncestorType.pure[IM]
    }

  private def collectErrorWithTypes(
      errorResponseSpecs: Map[StatusCode, SafeSchema]
  ) =
    errorResponseSpecs.toList
      .traverse { case (k, schema) =>
        model.schemaToType(schema).map(t => k -> t)
      }
      .map(_.toMap)

  private def collectSuccessCodesWithTypes(
      successResponseSpecs: Map[StatusCode, SafeSchema]
  ) =
    successResponseSpecs.toList
      .traverse { case (k, schema) =>
        model.schemaToType(schema).map(t => k -> t)
      }
      .map(_.toMap)

  private def getCommonAncestor(
      errorResponseSpecs: List[SafeSchema]
  ): IM[Option[Type]] =
    errorResponseSpecs match {
      case ::(head, Nil) =>
        model
          .schemaToType(head)
          .map(t => Some(t))
      case ::(head, tl) =>
        val errorAncestor = model
          .commonAncestor(
            NonEmptyList(head, tl).map(
              _.asInstanceOf[SafeRefSchema].ref
            ) //primitives can't be inherited
          )
          .head
        Option(model.classNameFor(errorAncestor).typeName: Type).pure[IM]
      case Nil => Option.empty[Type].pure[IM]
    }

}
