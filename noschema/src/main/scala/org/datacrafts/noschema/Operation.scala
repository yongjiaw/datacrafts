package org.datacrafts.noschema

import org.datacrafts.noschema.Context.LocalContext
import org.datacrafts.noschema.Operation.{DefaultFormatter, Formatter, Operator}

/**
  * For a specific schema context, construct the immutable operators from top down
  * @param context the schema encapsulated inside a context (path from the root)
  * @param rule the rule that defines the operator(marshaling/unmarshaling) given the context.
  *             note that the entire operation object, not just the context,
  *             needs to be passed as parameter of the rule,
  *             since for shapeless operators to do recursive marshaling/unmarshaling at runtime,
  *             it need to retrieve the dependencies' operators from the operation object
  * @tparam T
  */
final class Operation[T](
  val context: Context[T],
  val rule: Operation.Rule
) {

  val operator: Operation.Operator[T] = rule.getOperator(this)

  lazy val dependencyOperationMap: Map[Context.LocalContext[_], Operation[_]] =
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
          s"not found in ${dependencyOperationMap.keySet}. " +
          s"This should not happen under intended usage"
      )
    ).asInstanceOf[Operation[D]]
  }

  def format(
    formatter: Formatter = new DefaultFormatter(true)
  ): String = {
    formatter.formatRecursive(this, Set.empty, Seq.empty)
  }
}

/**
  * Companion object contains all the relevant traits
  */
object Operation {

  trait Formatter {

    def formatNode(node: NoSchema[_], operator: Operator[_]): String

    def formatDependency(
      depOp: Operation[_],
      openLevels: Set[Int],
      previousTypes: Seq[NoSchema.ScalaType[_]]): String

    implicit class OperationConverter(operation: Operation[_]) {
      def contextName: String = {
        operation.context.localContext match {
          case Context.MemberVariable(symbol, _) => symbol.name
          case Context.ContainerElement(_) => "element"
          case Context.CoproductElement(symbol, _) => s"${symbol.name}"
          case Context.Root(_) => throw new Exception(s"root should not trigger this")
        }
      }
    }

    final def formatRecursive(
      operation: Operation[_],
      openLevels: Set[Int],
      previousTypes: Seq[NoSchema.ScalaType[_]]
    ): String = {
      val level = previousTypes.size
      val currentType = operation.context.noSchema.scalaType
      if (! currentType.hasTypeArgs && previousTypes.contains(currentType)
      ) {
        s"${operation.context.noSchema.scalaType.fullName}" +
          s"(...cycle detected, the actual depth depends on runtime instantiation)\n"
      }
      else {
        s"${formatNode(operation.context.noSchema, operation.operator)}${

          val dependencies = operation.dependencyOperationMap.values.toSeq.sortBy(_.contextName)
          (0 until operation.dependencyOperationMap.size).map {
            i =>
              val op = dependencies(i)
              val depOpenLevels = if (i == dependencies.size - 1) {
                openLevels - level
              } else {
                openLevels + level
              }
              formatDependency(
                op,
                depOpenLevels,
                previousTypes :+ currentType
              )
          }.mkString("")

        }"
      }

    }
  }

  class DefaultFormatter(showOperator: Boolean) extends Formatter {
    override def formatNode(
      node: NoSchema[_],
      operator: Operator[_]
    ): String = {
      s"${node.scalaType.fullName}(${node.category}, nullable=${node.nullable})" +
        s"${if (showOperator) s" => ${operator}" else ""}\n"
    }

    override def formatDependency(
      depOp: Operation[_],
      openLevels: Set[Int],
      previousTypes: Seq[NoSchema.ScalaType[_]]
    ): String = {
      val level = previousTypes.size - 1
      s"${
        (0 to level - 1)
          .map(i => if (openLevels.contains(i)) " │  " else "   " ).mkString("") // scalastyle:ignore
      }${if (openLevels.contains(level)) " ├──" else " └──" // scalastyle:ignore
      }${depOp.contextName}: ${formatRecursive(depOp, openLevels, previousTypes)}"
    }
  }

  trait Rule {
    def getOperator[V](operation: Operation[V]): Operator[V]
  }

  trait Operator[T] {

    override def toString: String = s"${this.getClass.getSimpleName}" +
      s"${if (default.isDefined) s"(default=${default.get})" else ""}"

    def operation: Operation[T]

    final def marshal(input: Any): T = {
      if (Option(input).isDefined) {
        marshalNoneNull(input)
      } else {
        default.getOrElse(
          if (operation.context.noSchema.nullable) {
            null.asInstanceOf[T] // scalastyle:ignore
          } else {
            throw new Exception(
              s"input is null, but nullable=${operation.context.noSchema.nullable} " +
                s"and operator ${this} has no default")
          }
        )
      }
    }

    protected def marshalNoneNull(input: Any): T

    final def unmarshal(input: T): Any = {
      if (Option(input).isDefined) {
        unmarshalNoneNull(input)
      } else {
        null // scalastyle:ignore
      }
    }

    protected def unmarshalNoneNull(input: T): Any

    def default: Option[T] = None

  }

}
