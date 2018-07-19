package org.datacrafts.noschema.avro

import org.apache.avro.generic.GenericData
import org.datacrafts.noschema.{NoSchema, NoSchemaDsl, Operation, ShapelessCoproduct}
import org.datacrafts.noschema.operator.ShapelessCoproductOperator
import org.datacrafts.noschema.ShapelessCoproduct.TypeValueExtractor
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.CoproductBuilder

class ShapelessCoproductAvroOperator[T] (
  override val shapeless: ShapelessCoproduct[T, _],
  override val operation: Operation[T],
  avroRules: DefaultAvroRule
) extends ShapelessCoproductOperator[T, Any] with NoSchemaDsl {

  override protected def parse(input: Any): TypeValueExtractor = {
    new TypeValueExtractor {
      override def getTypeValue(tpe: NoSchema.ScalaType[_]): Option[Any] = {
        // only based on avro instance, there is no way to determine the type
        // enum and union of primitives can infer from value
        // for structured type such as case class, different types can have the same structure
        // just take the first one that matches

        throw new Exception("cannot determine type for coproduct")
      }
    }
  }

  private lazy val avroOperation = new AvroOperation(operation, avroRules)
  override protected def newCoproductBuilder(): CoproductBuilder[Any] = {
    new CoproductBuilder[Any] {
      private var _value: Option[(NoSchema.ScalaType[_], Any)] = None

      override def build(): Any = _value match {
        case Some((noschema, value)) =>
        if (avroRules.isEnum(shapeless)) {
          new GenericData.EnumSymbol(avroOperation.avroSchema, noschema.shortName)
        }
        else if (avroRules.isUnion(shapeless)) {
          value
        }
        else {
          throw new Exception(s"neither enum or union\n${shapeless.format()}")
        }
        case None =>
          // if this happens, there is a bug somewhere that failed to collect the symbol value,
          // which must exist with any scala instance
          throw new Exception(s"no value produced for coproduct, this is impossible")
      }

      override def addTypeValue(tpe: NoSchema.ScalaType[_],
        value: Any
      ): ShapelessCoproduct.UnionTypeValueCollector = {
        _value = Some((tpe, value))
        this
      }
    }
  }
}
