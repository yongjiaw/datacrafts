package org.datacrafts.noschema.operator

import scala.util.{Failure, Success, Try}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.ShapelessCoproduct
import org.datacrafts.noschema.ShapelessCoproduct.{TypeValueExtractor, UnionTypeValueCollector}
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.{CoproductBuilder, CoproductInfo}
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.NoSchema.ScalaType

abstract class ShapelessCoproductOperator[T, O] extends Operator[T] with Slf4jLogging.Default {

  protected def shapeless: ShapelessCoproduct[T, _]

  final protected def parse(input: Any): TypeValueExtractor = {
    new TypeValueExtractor {
      override def getTypeValue(coproductElement: CoproductElement[_]): Option[Any] = {
        matchInputWithCoproductElement(input, coproductElement)
      }
      override def toString: String = s"CoproductExtractor: input=${input}"
    }
  }

  def matchInputWithCoproductElement(input: Any, coproductElement: CoproductElement[_]): Option[Any]

  protected def coproductInfoToOutput(coproductInfo: CoproductInfo): O

  final protected def newCoproductBuilder(): CoproductBuilder[O] =
    new CoproductBuilder[O] {
    private var coproductInfo: Option[CoproductInfo] = None

    override def build(): O = coproductInfo.map(coproductInfoToOutput).getOrElse(
      throw new Exception("coproduct value is empty, this is impossible"))

    override def addTypeValue(coproductElement: CoproductElement[_],
      value: Any
    ): ShapelessCoproduct.UnionTypeValueCollector = {
      if (coproductInfo.isDefined) {
        throw new Exception(s"adding value for corpoduct should only be invoked once")
      }
      coproductInfo = Some(CoproductInfo(coproductElement, value))
      this
    }
  }

  protected final override def marshalNoneNull(input: Any): T = {
    val tag = operation.context.noSchema.scalaType.classTag
    val className = input.getClass.getCanonicalName
    input match {
      case tag(t) =>
        logDebug(s"input[${className}] is already expected type: " +
          s"${operation.context.noSchema.scalaType}")
        t
      case _ =>
        logDebug(s"input ${input}[${className}] is not expected type: " +
          s"${operation.context.noSchema.scalaType}, " +
          s"parse and perform shapeless transform")
        Try(parse(input)) match {
          case Success(parsed) => shapeless.marshal(parsed, operation)
          case Failure(f) => throw new Exception(
            s"failed to parse input $input for\n${operation.format()}", f)
        }
    }
  }

  protected final override def unmarshalNoneNull(input: T): O = {
    shapeless.unmarshal(input, newCoproductBuilder(), operation)
      .asInstanceOf[CoproductBuilder[O]].build()
  }
}

object ShapelessCoproductOperator {

  case class CoproductInfo(
    coproductElement: CoproductElement[_],
    value: Any
  )

  trait CoproductBuilder[O] extends UnionTypeValueCollector {
    def build(): O
  }

}
