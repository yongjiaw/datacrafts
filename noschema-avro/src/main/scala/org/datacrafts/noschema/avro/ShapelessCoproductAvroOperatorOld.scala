package org.datacrafts.noschema.avro

import org.apache.avro.generic.{GenericData, GenericRecordBuilder}
import org.datacrafts.noschema.{NoSchema, NoSchemaDsl, Operation, ShapelessCoproduct}
import org.datacrafts.noschema.ShapelessCoproduct.TypeValueExtractor
import org.datacrafts.noschema.operator.ShapelessCoproductOperator
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.CoproductBuilder
import org.datacrafts.noschema.Context.CoproductElement

class ShapelessCoproductAvroOperatorOld[T] (
  override val shapeless: ShapelessCoproduct[T, _],
  override val operation: Operation[T],
  val avroRule: DefaultAvroRule
) extends ShapelessCoproductOperator[T, Any] with NoSchemaDsl {

  override protected def parse(input: Any): TypeValueExtractor = {
    new TypeValueExtractor {
      override def getTypeValue(coproductElement: CoproductElement[_]): Option[Any] = {
        // only based on avro instance, there is no way to determine the type
        // enum and union of primitives can infer from value
        // for structured type such as case class, different types can have the same structure
        // just take the first one that matches

        throw new Exception("cannot determine type for coproduct")
      }
    }
  }

  private lazy val avroOperation = new AvroOperationOld(operation, avroRule)

  override protected def newCoproductBuilder(): CoproductBuilder[Any] = {
    new CoproductBuilder[Any] {
      private var _value: Option[(CoproductElement[_], Any)] = None

      override def build(): Any = _value match {
        case Some((coproductElement, value)) =>
        if (avroOperation.schemaInfo.isEnum) {
          new GenericData.EnumSymbol(
            avroOperation.avroSchema, coproductElement.noSchema.scalaType.shortName)
        }
        else if (avroOperation.schemaInfo.isUnion) {
          avroOperation.schemaInfo.getWrappedSchema(coproductElement) match {
            case Some(wrappedSchema) =>
              val record = new GenericData.Record(wrappedSchema)
              record.put(wrappedSchema.getFields.get(0).name(), value)
              record
            case None => value
          }
        }
        else {
          throw new Exception(s"neither enum nor union\n${shapeless.format()}")
        }
        case None =>
          // if this happens, there is a bug somewhere that failed to collect the symbol value,
          // which must exist with any scala instance
          throw new Exception(s"no value produced for coproduct, this is impossible")
      }

      override def addTypeValue(coproductElement: CoproductElement[_],
        value: Any
      ): ShapelessCoproduct.UnionTypeValueCollector = {
        _value = Some((coproductElement, value))
        this
      }
    }
  }
}
