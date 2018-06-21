package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{Operation, ShapelessProduct}
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.ShapelessProduct.{SymbolCollector, SymbolExtractor}
import org.datacrafts.noschema.operator.ShapelessProductOperator.SymbolBuilder

abstract class ShapelessProductOperator[T, I, O](
  shapeless: ShapelessProduct[T, _],
  override val operation: Operation[T]
) extends Operator[T] {

  protected def parse(input: I): SymbolExtractor

  protected def newSymbolBuilder(): SymbolBuilder[O]

  final override def marshal(input: Any): T = {
    shapeless.marshal(parse(input.asInstanceOf[I]), operation)
  }

  final override def unmarshal(input: T): O = {
    shapeless.unmarshal(input, newSymbolBuilder(), operation)
      .asInstanceOf[SymbolBuilder[O]].build()
  }
}

object ShapelessProductOperator {

  trait SymbolBuilder[O] extends SymbolCollector {
    def build(): O
  }

}
