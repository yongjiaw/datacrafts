package org.datacrafts.noschema

import org.datacrafts.noschema.operator.ProductOperator.{SymbolCollector, SymbolExtractor}

abstract class NoSchemaProduct[T: NoSchema.ScalaType](
  val fields: Seq[Context.MemberVariable[_]]
) extends NoSchema[T] (
  category = NoSchema.Category.Product,
  nullable = true) {

  def marshal(symbolExtractor: SymbolExtractor, operation: Operation[T]): T

  def unmarshal(input: T, emptyCollector: SymbolCollector, operation: Operation[T]): SymbolCollector
}
