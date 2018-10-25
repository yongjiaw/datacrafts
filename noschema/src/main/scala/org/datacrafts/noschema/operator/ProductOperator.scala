package org.datacrafts.noschema.operator

import scala.util.{Failure, Success, Try}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.NoSchemaProduct
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.operator.ProductOperator.{ProductBuilder, SymbolExtractor}
import org.datacrafts.noschema.Context.MemberVariable

abstract class ProductOperator[T, O] extends Operator[T] with Slf4jLogging.Default {

  protected def product: NoSchemaProduct[T]

  protected def parse(input: Any): SymbolExtractor

  protected def newProductBuilder(): ProductBuilder[O]

  protected final override def marshalNoneNull(input: Any): T = {
    val className = input.getClass.getCanonicalName
    operation.context.noSchema.scalaType.matchInput(input) match {
        // this match might be very expensive
      case Some(t) =>
        logDebug(s"input[${className}] is already expected type: " +
          s"${operation.context.noSchema.scalaType}")
        t
      case _ =>
        logDebug(s"input ${input}[${className}] is not expected type: " +
          s"${operation.context.noSchema.scalaType}, " +
          s"parse and perform shapeless transform")
        Try(parse(input)) match {
          case Success(parsed) => product.marshal(parsed, operation)
          case Failure(f) => throw new Exception(
            s"failed to parse input $input for\n${operation.format()}", f)
        }
    }
  }

  protected final override def unmarshalNoneNull(input: T): O = {
    product.unmarshal(input, newProductBuilder(), operation)
      .asInstanceOf[ProductBuilder[O]].build()
  }
}

object ProductOperator {

  trait ProductBuilder[O] extends SymbolCollector {
    def build(): O
  }

  trait SymbolExtractor {
    def removeSymbol(symbol: MemberVariable[_]): SymbolExtractor

    // can control the whether the symbol is allowed to be absent and treated as null
    def getSymbolValue(symbol: MemberVariable[_]): Any

    def allSymbolsExtracted(): Unit
  }

  trait SymbolCollector {

    // add symbol value pairs disassembled from the structured class
    // can control the behavior when the symbol has null or empty value
    def addSymbolValue(symbol: MemberVariable[_], value: Any): SymbolCollector

  }

}
