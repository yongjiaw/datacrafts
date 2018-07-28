package org.datacrafts.noschema.avro

import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord, GenericRecordBuilder}
import org.datacrafts.noschema.{Operation, ShapelessProduct}
import org.datacrafts.noschema.ShapelessProduct.SymbolExtractor
import org.datacrafts.noschema.operator.ShapelessProductOperator
import org.datacrafts.noschema.operator.ShapelessProductOperator.ProductBuilder
import org.datacrafts.noschema.Context.MemberVariable

class ShapelessProductAvroOperatorOld[T](
  override val shapeless: ShapelessProduct[T, _],
  override val operation: Operation[T],
  val avroRule: DefaultAvroRule
) extends ShapelessProductOperator[T, GenericRecord] {

  override protected def parse(input: Any): ShapelessProduct.SymbolExtractor = {
    input match {
      case record: GenericRecord =>
        new SymbolExtractor {

          override def removeSymbol(symbol: MemberVariable[_]): SymbolExtractor = this

          override def getSymbolValue(symbol: MemberVariable[_]): Any =
            record.get(symbol.symbol.name)

          override def allSymbolsExtracted(): Unit = {}
        }
      case _ => throw new Exception(s"input type ${input.getClass} is not GenericRecord")
    }
  }

  private lazy val avroOperation = new AvroOperationOld(operation, avroRule)

  override protected def newProductBuilder(): ProductBuilder[GenericRecord] =
    new ProductBuilder[GenericRecord] {

      // it's faster to direct build Record than using the RecordBuilder
      // RecordBuilder does some extra validation
      val record = new GenericData.Record(avroOperation.avroSchema)

      override def build(): GenericRecord = record

      override def addSymbolValue(member: MemberVariable[_],
        value: Any
      ): ShapelessProduct.SymbolCollector = {

        if (Option(value).isDefined || avroOperation.schemaInfo.memberIsNullable(member)) {
          record.put(member.symbol.name, value)
        }
        else {
          val depOp = operation.dependencyOperation(member)
          throw new Exception(s"${shapeless.scalaType.fullName}: " +
            s"${member.symbol}=${depOp.context.noSchema.scalaType.uniqueKey} " +
            s"encountered undefined value ${value}, " +
            s"use Option[] or change schema rule to accept null")
        }
        this
      }
    }
}
