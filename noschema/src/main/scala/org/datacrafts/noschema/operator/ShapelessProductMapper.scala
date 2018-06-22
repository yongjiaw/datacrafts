package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{Operation, ShapelessProduct}
import org.datacrafts.noschema.ShapelessProduct.{SymbolCollector, SymbolExtractor}
import org.datacrafts.noschema.operator.ShapelessProductOperator.SymbolBuilder

class ShapelessProductMapper[T](
  override val operation: Operation[T],
  override val shapeless: ShapelessProduct[T, _],
  allowUnknownField: Boolean = false
) extends ShapelessProductOperator[T, Iterable[(_, _)], Map[String, Any]]{

  override protected def parse(input: Iterable[(_, _)]): SymbolExtractor =
  new SymbolExtractor {
    private val map = collection.mutable.Map.empty[String, Any] ++
      input.map{ case(k, v) => (k.toString -> v)}

    override def removeSymbol(symbol: Symbol): SymbolExtractor = {
      map -= symbol.name
      this
    }

    override def getSymbolValue(symbol: Symbol): Any = {
      map.getOrElse(symbol.name, null) //scalastyle:ignore
    }

    override def allSymbolsExtracted(): Unit = {
      if (!allowUnknownField && map.nonEmpty) {
        throw new Exception(
          s"there are unknown fields [${map.keySet.mkString(",")}], " +
            s"context=${operation.context}")
      }
    }
  }

  override protected def newSymbolBuilder(): SymbolBuilder[Map[String, Any]] =
    new SymbolBuilder[Map[String, Any]]{
      private val symbolMap = collection.mutable.Map.empty[Symbol, Any]

      override def build(): Map[String, Any] = symbolMap.map {
        case (k, v) => k.name -> v
      }.toMap

      override def addSymbolValue(symbol: Symbol, value: Any): SymbolCollector = {
        symbolMap += symbol -> value
        this
      }
    }

}
