package io.github.ghostbuster91.sttp.client3

import io.github.ghostbuster91.sttp.client3.openapi._

class ReferenceCollector(model: Model) {
  def collect(operations: List[SafeOperation]): Set[SchemaRef] = {
    val refs = operations.toSet.flatMap(collectSingleOperation)
    val parentRefs = model.childToParentRef
      .filterKeys(refs.contains)
      .values
      .flatten
      .toSet
    refs ++ parentRefs
  }

  private def collectSingleOperation(
      operation: SafeOperation
  ): Set[SchemaRef] =
    collectRequestBody(operation) ++ collectParams(operation) ++
      collectResponses(operation)

  private def collectResponses(operation: SafeOperation) =
    operation.responses.values.toList
      .flatMap(_.content.values.toList)
      .flatMap(mt => collectFromSchema(mt.schema))

  private def collectParams(operation: SafeOperation) =
    operation.parameters.flatMap(param => collectFromSchema(param.schema))

  private def collectRequestBody(operation: SafeOperation): Set[SchemaRef] =
    operation.requestBody.toList
      .flatMap(_.content.values)
      .flatMap(mt => collectFromSchema(mt.schema))
      .toSet

  private def collectFromSchema(schema: SafeSchema): Set[SchemaRef] =
    collectFromSchemaRec(schema, Set.empty)

  private def collectFromSchemaRec(
      schema: SafeSchema,
      acc: Set[SchemaRef]
  ): Set[SchemaRef] =
    schema match {
      case ref: SafeRefSchema =>
        if (!acc.contains(ref.ref)) {
          collectFromSchemaRec(model.schemas(ref.ref), acc + ref.ref)
        } else {
          acc
        }
      case obj: SchemaWithProperties =>
        obj.properties.values.foldLeft(acc)((acc2, item) =>
          collectFromSchemaRec(item, acc2)
        )
      case coproduct: SafeComposedSchema =>
        (coproduct.oneOf ++ coproduct.allOf).foldLeft(acc)((acc2, item) =>
          collectFromSchemaRec(item, acc2)
        )
      case arr: SafeArraySchema => collectFromSchemaRec(arr.items, acc)
      case _                    => acc
    }
}
