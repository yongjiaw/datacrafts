package org.datacrafts.noschema

import org.datacrafts.noschema.operator.CoproductOperator.{TypeValueExtractor, UnionTypeValueCollector}

abstract class NoSchemaCoproduct[T: NoSchema.ScalaType](
  val members: Seq[Context.CoproductElement[_]]
) extends NoSchema[T] (
  category = NoSchema.Category.CoProduct,
  nullable = true) {

  def marshal(typeExtractor: TypeValueExtractor, operation: Operation[T]): T

  def unmarshal(input: T, emptyUnion: UnionTypeValueCollector, operation: Operation[T]
  ): UnionTypeValueCollector
}
