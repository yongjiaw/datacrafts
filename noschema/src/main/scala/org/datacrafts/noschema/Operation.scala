package org.datacrafts.noschema

import org.datacrafts.noschema.Context.LocalContext

final class Operation[T](
  context: Context[T],
  operatorRule: Operation.OperatorRule
) {

  val operator: Operation.Operator[T] = operatorRule.getOperator(context)

  val dependencyOperationMap: Map[Context.LocalContext[_], Operation[_]] =
    context.noSchema.dependencies.map {
      dependency =>
        dependency -> new Operation(context.dependencyContext(dependency), operatorRule)
    }.toMap

  // structurally, this is only invoked through shapeless determined at compile time
  // by implicit resolution
  // user code can control the actual operations through the operatorRule
  def dependencyOperation[D](dependency: LocalContext[D]): Operation[D] = {
    dependencyOperationMap.getOrElse(
      dependency,
      throw new Exception(
        s"calling with unrecognized dependency ${dependency}, " +
          s"not found in ${dependencyOperationMap.keySet}"
      )
    ).asInstanceOf[Operation[D]]
  }
}

object Operation {

  trait OperatorRule {
    def getOperator[V](context: Context[V]): Operation.Operator[V]
  }

  trait Operator[T] {

    def operation: Operation[T]

    def marshal(input: Any): T

    def unmarshal(input: T): Any

  }

}
