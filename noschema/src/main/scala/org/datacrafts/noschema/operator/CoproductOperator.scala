package org.datacrafts.noschema.operator

import scala.util.{Failure, Success, Try}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.NoSchemaCoproduct
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.operator.CoproductOperator.{CoproductBuilder, CoproductInfo, TypeValueExtractor, UnionTypeValueCollector}

abstract class CoproductOperator[T, O] extends Operator[T] with Slf4jLogging.Default {

  protected def coproduct: NoSchemaCoproduct[T]

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
    ): UnionTypeValueCollector = {
      if (coproductInfo.isDefined) {
        throw new Exception(s"adding value for corpoduct should only be invoked once")
      }
      coproductInfo = Some(CoproductInfo(coproductElement, value))
      this
    }
  }

  protected final override def marshalNoneNull(input: Any): T = {
    val className = input.getClass.getCanonicalName
    operation.context.noSchema.scalaType.matchInput(input) match {
      case Some(value) =>
        logDebug(s"input[${className}]($input) is already expected type: " +
          s"${operation.context.noSchema.scalaType}")
        value
      case None =>
        logDebug(s"input ${input}[${className}] is not expected type: " +
          s"${operation.context.noSchema.scalaType}, " +
          s"parse and perform shapeless transform")
        Try(parse(input)) match {
          case Success(parsed) => coproduct.marshal(parsed, operation)
          case Failure(f) => throw new Exception(
            s"failed to parse input $input for\n${operation.format()}", f)
        }
    }
  }

  protected final override def unmarshalNoneNull(input: T): O = {
    coproduct.unmarshal(input, newCoproductBuilder(), operation)
      .asInstanceOf[CoproductBuilder[O]].build()
  }
}

object CoproductOperator {

  case class CoproductInfo(
    coproductElement: CoproductElement[_],
    value: Any
  )

  trait CoproductBuilder[O] extends UnionTypeValueCollector {
    def build(): O
  }

  trait TypeValueExtractor {
    // can control the whether the symbol is allowed to be absent and treated as null
    def getTypeValue(coproductElement: CoproductElement[_]): Option[Any]
  }

  trait UnionTypeValueCollector {
    def addTypeValue(coproductElement: CoproductElement[_], value: Any): UnionTypeValueCollector
  }
}
