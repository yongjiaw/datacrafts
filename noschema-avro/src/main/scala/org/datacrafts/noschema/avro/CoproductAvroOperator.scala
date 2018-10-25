package org.datacrafts.noschema.avro

import org.apache.avro.generic.GenericData
import org.apache.avro.generic.GenericData.EnumSymbol
import org.datacrafts.noschema.{NoSchemaCoproduct, NoSchemaDsl}
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.operator.CoproductOperator
import org.datacrafts.noschema.operator.CoproductOperator.CoproductInfo

class CoproductAvroOperator[T] (
  override val coproduct: NoSchemaCoproduct[T],
  override val operation: AvroOperation[T],
  val avroRule: AvroRule
) extends CoproductOperator[T, Any] with NoSchemaDsl {

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
      throw new Exception(s"neither enum nor union\n${coproduct.format()}")
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
      throw new Exception(s"neither enum nor union\n${coproduct.format()}")
    }
  }
}
