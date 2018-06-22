package org.datacrafts.noschema

import org.datacrafts.noschema.Context.LocalContext
import org.datacrafts.noschema.Operation.Operator

final class Operation[T](
  val context: Context[T],
  val rule: Operation.Rule
) {

  val operator: Operation.Operator[T] = rule.getOperator(this)

  val dependencyOperationMap: Map[Context.LocalContext[_], Operation[_]] =
    context.noSchema.dependencies.map {
      dependency =>
        dependency -> new Operation(context.dependencyContext(dependency), rule)
    }.toMap

  // this is invoked through implicit resolution at compile time based on structure of the type
  // user code can control the actual operations through the rule
  def dependencyOperation[D](dependency: LocalContext[D]): Operation[D] = {
    dependencyOperationMap.getOrElse(
      dependency,
      throw new Exception(
        s"calling with unrecognized dependency ${dependency}, " +
          s"not found in ${dependencyOperationMap.keySet}"
      )
    ).asInstanceOf[Operation[D]]
  }

  def format(
    schemaFormatter: (NoSchema[_], Operator[_]) => String = {
      (noSchema: NoSchema[_], operator: Operator[_]) =>
        s"${noSchema} => ${operator.getClass.getSimpleName}\n"
    },
    dependencyFormatter: (Operation[_], String) => String = {
      (depOp: Operation[_], depFormatted) => s"${"\t" * depOp.context.level}${
        depOp.context.localContext match {
          case Context.MemberVariable(symbol, _) => symbol.map(_.name).get
          case Context.ContainerElement(_) => "element"
        }
      }: ${depFormatted}"
    }
  ): String = {
    s"${schemaFormatter(context.noSchema, operator)}${
      dependencyOperationMap.values.map {
        op => dependencyFormatter(op, s"${op.format(schemaFormatter, dependencyFormatter)}")
      }.mkString("")}"
  }
}

object Operation {

  trait Rule {
    def getOperator[V](operation: Operation[V]): Operator[V]
  }

  trait Operator[T] {

    def operation: Operation[T]

    def marshal(input: Any): T = {
      if (Option(input).isDefined) {
        marshalNoneNull(input)
      } else if (operation.context.noSchema.nullable) {
        null.asInstanceOf[T] // scalastyle:ignore
      } else {
        default.getOrElse(
          throw new Exception(
            s"input is null, but ${operation.context.noSchema} is not nullable, " +
              s"and operator ${this} has no default"))
      }
    }

    protected def marshalNoneNull(input: Any): T

    def unmarshal(input: T): Any = {
      if (Option(input).isDefined) {
        unmarshalNoneNull(input)
      } else {
        input
      }
    }

    protected def unmarshalNoneNull(input: T): Any

    def default: Option[T] = None

  }

}
