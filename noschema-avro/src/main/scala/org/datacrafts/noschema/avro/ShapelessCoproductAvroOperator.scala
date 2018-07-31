package org.datacrafts.noschema.avro

import org.apache.avro.generic.{GenericData, GenericRecord}
import org.apache.avro.generic.GenericData.EnumSymbol
import org.datacrafts.noschema.{NoSchema, NoSchemaDsl, Primitive, ShapelessCoproduct}
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.ShapelessCoproduct.TypeValueExtractor
import org.datacrafts.noschema.avro.AvroRule.SchemaWrapper
import org.datacrafts.noschema.operator.{PrimitiveOperator, ShapelessCoproductOperator}
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.CoproductBuilder

class ShapelessCoproductAvroOperator[T] (
  override val shapeless: ShapelessCoproduct[T, _],
  override val operation: AvroOperation[T],
  val avroRule: AvroRule
) extends ShapelessCoproductOperator[T, Any] with NoSchemaDsl {

  override protected def parse(input: Any): TypeValueExtractor = {
    new TypeValueExtractor {
      override def toString: String = s"Extractor(${input})"
      override def getTypeValue(coproductElement: CoproductElement[_]): Option[Any] = {
        // only based on avro instance, there is no way to determine the type
        // enum and union of primitives can infer from value
        // for structured type such as case class, different types can have the same structure
        // just take the first one that matches
        if (operation.isEnum) {
          input match {
            case enum: EnumSymbol =>
              if (coproductElement.symbol.name == enum.toString) {
                Some(input)
              }
              else {
                None
              }
            case _ => throw new Exception(
              s"unknown input type for Enum operation ${input.getClass}: $input")
          }
        }
        else if (operation.isUnion) {
          Some(input)
        }
        else {
          throw new Exception(s"neither enum nor union\n${shapeless.format()}")
        }

      }
    }
  }

  override protected def newCoproductBuilder(): CoproductBuilder[Any] = {
    new CoproductBuilder[Any] {
      private var _value: Option[(CoproductElement[_], Any)] = None

      override def build(): Any = _value match {
        case Some((coproductElement, value)) =>
          if (operation.isEnum) {
            new GenericData.EnumSymbol(
              operation.originalSchema, avroRule.getEnumValue(coproductElement))
          }
          else if (operation.isUnion) {
            value
          }
          else {
            throw new Exception(s"neither enum nor union\n${shapeless.format()}")
          }
        case None =>
          // if this happens, there is a bug somewhere that failed to collect the symbol value,
          // or this method is invoked in some unexpected way
          // the value must exist with any scala instance
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
