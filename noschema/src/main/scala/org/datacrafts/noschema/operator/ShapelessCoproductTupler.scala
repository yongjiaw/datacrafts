package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{NoSchema, Operation, ShapelessCoproduct, ShapelessProduct}
import org.datacrafts.noschema.ShapelessCoproduct.TypeValueExtractor
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.CoproductBuilder

class ShapelessCoproductTupler[T](
  override val operation: Operation[T],
  override val shapeless: ShapelessCoproduct[T, _]
) extends ShapelessCoproductOperator [T, (_, _), (String, Any)]{

  override protected def parse(input: Tuple2[_, _]): TypeValueExtractor = new TypeValueExtractor {
    val (key, value) = input
    override def getTypeValue(tpe: NoSchema.ScalaType[_]): Option[Any] = {
      if (s"$key" == tpe.tpe.toString) Some(value) else None
    }
  }

  override protected def newCoproductBuilder(): CoproductBuilder[(String, Any)] =
    new CoproductBuilder[(String, Any)] {
      private var tuple: Option[(String, Any)] = None

      override def build(): (String, Any) = tuple.getOrElse(
        throw new Exception("union type value is empty"))

      override def addTypeValue(tpe: NoSchema.ScalaType[_],
        value: Any
      ): ShapelessCoproduct.UnionTypeValueCollector = {
        if (tuple.isDefined) {
          throw new Exception(s"adding value for corpoduct should only be invoked once")
        }
        tuple = Some((tpe.tpe.toString, value))
        this
      }
    }
}
