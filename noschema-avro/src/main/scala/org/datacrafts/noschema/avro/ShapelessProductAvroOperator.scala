package org.datacrafts.noschema.avro

import org.apache.avro.Schema
import org.apache.avro.generic.{GenericRecord, GenericRecordBuilder}
import org.datacrafts.noschema.{Operation, ShapelessProduct}
import org.datacrafts.noschema.ShapelessProduct.SymbolExtractor
import org.datacrafts.noschema.operator.ShapelessProductOperator
import org.datacrafts.noschema.operator.ShapelessProductOperator.ProductBuilder

class ShapelessProductAvroOperator[T](
  override val shapeless: ShapelessProduct[T, _],
  override val operation: Operation[T],
  val avroRule: DefaultAvroRule
) extends ShapelessProductOperator[T, GenericRecord] {

  override protected def parse(input: Any): ShapelessProduct.SymbolExtractor = {
    input match {
      case record: GenericRecord =>
        new SymbolExtractor {

          override def removeSymbol(symbol: Symbol): SymbolExtractor = this

          override def getSymbolValue(symbol: Symbol): Any = record.get(symbol.name)

          override def allSymbolsExtracted(): Unit = {}
        }
      case _ => throw new Exception(s"input type ${input.getClass} is not GenericRecord")
    }
  }

  private lazy val avroOperation = new AvroOperation(operation, avroRule)

  override protected def newProductBuilder(): ProductBuilder[GenericRecord] =
    new ProductBuilder[GenericRecord] {
      val recordBuilder = new GenericRecordBuilder(avroOperation.avroSchema)

      override def build(): GenericRecord = recordBuilder.build()

      override def addSymbolValue(symbol: Symbol,
        value: Any
      ): ShapelessProduct.SymbolCollector = {
        // if the avro schema is not a union with null but the value is null,
        // recordBuilder will fail at build

        if (!Option(value).isDefined) {
          val fieldSchema = avroOperation.avroSchema.getField(symbol.name).schema()
          import scala.collection.JavaConverters._
          if (fieldSchema.getType == Schema.Type.UNION &&
            fieldSchema.getTypes.asScala.exists(_.getType == Schema.Type.NULL)) {
              recordBuilder.set(symbol.name, value)
          }
          else {
            val fieldClass =
            shapeless.dependencies.collectFirst{
              case m if m.symbol == symbol => m.noSchema.scalaType
            }.getOrElse(throw new Exception(s"${symbol.name} does not found among dependencies " +
              s"${shapeless.dependencies}, this is not possible with intended use"))

            throw new Exception(s"${shapeless.scalaType.fullName}: " +
              s"${symbol}=${fieldClass.uniqueKey} encountered null value, " +
              s"use Option[${fieldClass.uniqueKey}] or change schema rule to accept null")
          }
        }
        else {
          recordBuilder.set(symbol.name, value)
        }

        this
      }
    }
}
