package org.datacrafts.noschema.avro

import org.apache.avro.generic.{GenericData, GenericRecord}
import org.datacrafts.noschema.Context.MemberVariable
import org.datacrafts.noschema.ShapelessProduct
import org.datacrafts.noschema.ShapelessProduct.SymbolExtractor
import org.datacrafts.noschema.operator.ShapelessProductOperator
import org.datacrafts.noschema.operator.ShapelessProductOperator.ProductBuilder

class ShapelessProductAvroOperator[T](
  override val shapeless: ShapelessProduct[T, _],
  override val operation: AvroOperation[T],
  val avroRule: AvroRule
) extends ShapelessProductOperator[T, GenericRecord] {

  override protected def parse(input: Any): ShapelessProduct.SymbolExtractor = {
    input match {
      case record: GenericRecord =>
        new SymbolExtractor {
          override def removeSymbol(member: MemberVariable[_]): SymbolExtractor = this

          override def getSymbolValue(member: MemberVariable[_]): Any = {
            val result = record.get(member.symbol.name)
            if (Option(result).isEmpty && !operation.dependencyOperation(member).isNullable) {
              throw new Exception(s"failed to marshal ${shapeless.scalaType.fullName} " +
                s"${member.symbol.name} cannot be null: $input")
            }
            result
          }

          override def allSymbolsExtracted(): Unit = {}
        }
      // enum is just empty struct
      case enum: GenericData.EnumSymbol =>
        new SymbolExtractor {

          override def removeSymbol(symbol: MemberVariable[_]): SymbolExtractor = this

          override def getSymbolValue(symbol: MemberVariable[_]): Any = null // scalastyle:ignore

          override def allSymbolsExtracted(): Unit = {}
        }
      case _ => throw new Exception(s"unknown input type ${input.getClass}: $input")
    }
  }

  override protected def newProductBuilder(): ProductBuilder[GenericRecord] =
    new ProductBuilder[GenericRecord] {

      // it's faster to direct build Record than using the RecordBuilder
      // RecordBuilder does some extra validation
      val record = new GenericData.Record(operation.originalSchema)

      override def build(): GenericRecord = record

      override def addSymbolValue(member: MemberVariable[_],
        value: Any
      ): ShapelessProduct.SymbolCollector = {

        lazy val depOp = operation.dependencyOperation(member)
        if (Option(value).isDefined || depOp.isNullable) {
          record.put(member.symbol.name, value)
        }
        else {
          throw new Exception(s"${shapeless.scalaType.fullName}: " +
            s"${member.symbol}=${depOp.context.noSchema.scalaType.uniqueKey} " +
            s"encountered undefined value ${value}, " +
            s"use Option[] or change schema rule to accept null")
        }
        this
      }
    }
}
