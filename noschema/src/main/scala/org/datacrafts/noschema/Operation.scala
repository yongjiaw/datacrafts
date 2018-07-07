package org.datacrafts.noschema

import org.datacrafts.noschema.Context.LocalContext
import org.datacrafts.noschema.Operation.{DefaultFormatter, Formatter, Operator}

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
    formatter: Formatter = new DefaultFormatter(true)
  ): String = {
    formatter.formatRecursive(this, Set.empty)
  }
}

object Operation {

  trait Formatter {

    def formatNode(node: NoSchema[_], operator: Operator[_]): String

    def formatDependency(depOp: Operation[_], openLevels: Set[Int]): String

    implicit class OperationConverter(operation: Operation[_]) {
      def contextName: String = {
        operation.context.localContext match {
          case Context.MemberVariable(symbol, _) => symbol.map(_.name).get
          case Context.ContainerElement(_) => "element"
          case Context.CoproductElement(symbol, noSchema) => s"${noSchema.scalaType.tpe}"
        }
      }
    }

    final def formatRecursive(operation: Operation[_], openLevels: Set[Int]): String = {
      s"${formatNode(operation.context.noSchema, operation.operator)}${
        val dependencies = operation.dependencyOperationMap.values.toSeq.sortBy(_.contextName)
        (0 until operation.dependencyOperationMap.size).map {
          i =>
            val op = dependencies(i)
            val depOpenLevels = if (i == dependencies.size - 1) {
              openLevels - op.context.level
            } else {
              openLevels + op.context.level
            }
            formatDependency(
              op,
              depOpenLevels
            )
        }.mkString("")}"
    }
  }

  class DefaultFormatter(showOperator: Boolean) extends Formatter {
    override def formatNode(
      node: NoSchema[_],
      operator: Operator[_]
    ): String = {
      s"${node.scalaType.tpe.typeSymbol.name}(${node.category}, nullable=${node.nullable})" +
        s"${if (showOperator) s" => ${operator}" else ""}\n"
    }

    override def formatDependency(depOp: Operation[_],
      openLevels: Set[Int]
    ): String = {
      s"${
        (0 to depOp.context.level - 1)
          .map(i => if (openLevels.contains(i)) " │  " else "   " ).mkString("") // scalastyle:ignore
      }${if (openLevels.contains(depOp.context.level)) " ├──" else " └──" // scalastyle:ignore
      }${depOp.contextName}: ${formatRecursive(depOp, openLevels)}"
    }
  }

  trait Rule {
    def getOperator[V](operation: Operation[V]): Operator[V]
  }

  trait Operator[T] {

    override def toString: String = s"${this.getClass.getSimpleName}" +
      s"(allowNull=${allowNull}${if (!allowNull) s",default=${default}" else ""})"

    def operation: Operation[T]

    final def marshal(input: Any): T = {
      if (Option(input).isDefined) {
        marshalNoneNull(input)
      } else if (operation.context.noSchema.nullable && allowNull) {
        null.asInstanceOf[T] // scalastyle:ignore
      } else {
        default.getOrElse(
          throw new Exception(
            s"input is null, but nullable=${operation.context.noSchema.nullable} " +
              s"and allowNull=${allowNull}, " +
              s"and operator ${this} has no default"))
      }
    }

    protected def marshalNoneNull(input: Any): T

    final def unmarshal(input: T): Any = {
      if (Option(input).isDefined) {
        unmarshalNoneNull(input)
      } else if (allowNull) {
        input
      } else {
        throw new Exception(s"operator ${this} does not allow null value")
      }
    }

    protected def unmarshalNoneNull(input: T): Any

    def default: Option[T] = None

    def allowNull: Boolean = true

  }

}
