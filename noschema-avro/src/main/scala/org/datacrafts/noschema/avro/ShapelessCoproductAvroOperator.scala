package org.datacrafts.noschema.avro

import org.apache.avro.generic.{GenericData, GenericRecord}
import org.apache.avro.generic.GenericData.EnumSymbol
import org.datacrafts.noschema.{NoSchema, NoSchemaDsl, Primitive, ShapelessCoproduct}
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.ShapelessCoproduct.TypeValueExtractor
import org.datacrafts.noschema.avro.AvroRule.SchemaWrapper
import org.datacrafts.noschema.operator.{PrimitiveOperator, ShapelessCoproductOperator}
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.{CoproductBuilder, CoproductInfo}

class ShapelessCoproductAvroOperator[T] (
  override val shapeless: ShapelessCoproduct[T, _],
  override val operation: AvroOperation[T],
  val avroRule: AvroRule
) extends ShapelessCoproductOperator[T, Any] with NoSchemaDsl {

  override def matchInputWithCoproductElement(
    input: Any,
    coproductElement: CoproductElement[_]
  ): Option[Any] = {
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

  override def coproductInfoToOutput(coproductInfo: CoproductInfo): Any = {
    if (operation.isEnum) {
      new GenericData.EnumSymbol(
        operation.originalSchema, avroRule.getEnumValue(coproductInfo.coproductElement))
    }
    else if (operation.isUnion) {
      coproductInfo.value
    }
    else {
      throw new Exception(s"neither enum nor union\n${shapeless.format()}")
    }
  }
}
