package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{Operation, ShapelessProduct}
import org.datacrafts.noschema.ShapelessProduct.{SymbolCollector, SymbolExtractor}
import org.datacrafts.noschema.operator.ShapelessProductOperator.ProductBuilder

class ShapelessProductMapper[T](
  override val operation: Operation[T],
  override val shapeless: ShapelessProduct[T, _],
  allowUnknownField: Boolean = false,
  allowAbsence: Boolean = true
) extends ShapelessProductOperator[T, Map[String, Any]] {

  override protected def parse(input: Any): SymbolExtractor = input match {
    case iterable: Iterable[_] =>
      new SymbolExtractor {
        private val map = collection.mutable.Map.empty[String, Any] ++
          iterable.map {
            case value => value match {
              case (k, v) => (s"$k" -> v)
              case _ => throw new Exception(s"$value does not match key value pair")
            }
          }

        override def removeSymbol(symbol: Symbol): SymbolExtractor = {
          map -= symbol.name
          this
        }

        override def getSymbolValue(symbol: Symbol): Any = {
          map.getOrElse(
            symbol.name,
            if (allowAbsence) {
              null //scalastyle:ignore
            } else {
              throw new Exception(s"${symbol.name} is absent for ${shapeless.scalaType}")
            }
          )
        }

        override def allSymbolsExtracted(): Unit = {
          if (!allowUnknownField && map.nonEmpty) {
            throw new Exception(
              s"there are unknown fields [${map.keySet.mkString(",")}], " +
                s"context=${operation.context}")
          }
        }
      }
    case _ => throw new Exception(s"input type ${input.getClass} is not Iterable[(_, _)]")
  }

  override protected def newProductBuilder(): ProductBuilder[Map[String, Any]] =
    new ProductBuilder[Map[String, Any]] {
      private val symbolMap = collection.mutable.Map.empty[Symbol, Any]

      override def build(): Map[String, Any] = symbolMap.map {
        case (k, v) => k.name -> v
      }.toMap

      override def addSymbolValue(symbol: Symbol,
        value: Any
      ): SymbolCollector = {
        symbolMap += symbol -> value
        this
      }
    }

}
