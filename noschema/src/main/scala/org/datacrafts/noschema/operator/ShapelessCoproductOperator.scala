package org.datacrafts.noschema.operator

import scala.util.{Failure, Success, Try}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.ShapelessCoproduct
import org.datacrafts.noschema.ShapelessCoproduct.{TypeValueExtractor, UnionTypeValueCollector}
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.CoproductBuilder

abstract class ShapelessCoproductOperator[T, O] extends Operator[T] with Slf4jLogging.Default {

  protected def shapeless: ShapelessCoproduct[T, _]

  protected def parse(input: Any): TypeValueExtractor

  protected def newCoproductBuilder(): CoproductBuilder[O]

  protected final override def marshalNoneNull(input: Any): T = {
    val className = input.getClass.getCanonicalName
    if (className == operation.context.noSchema.scalaType.tpe.toString) {
      logDebug(s"input is already expected type: ${operation.context.noSchema.scalaType}")
      input.asInstanceOf[T]
    } else {
      logDebug(s"input ${input}[${className}] is not expected type: " +
        s"${operation.context.noSchema.scalaType.tpe}, parse and perform shapeless transform")
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

  trait CoproductBuilder[O] extends UnionTypeValueCollector {
    def build(): O
  }

}
