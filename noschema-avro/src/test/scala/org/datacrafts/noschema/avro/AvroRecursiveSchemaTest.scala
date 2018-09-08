package org.datacrafts.noschema.avro

import java.io.ByteArrayOutputStream

import org.apache.avro.{Schema, SchemaBuilder}
import org.apache.avro.SchemaBuilder.FieldBuilder
import org.apache.avro.generic.GenericData
import org.datacrafts.noschema.NoSchemaTest.{GenericType, Recursive, TestClass, TestClass3}

// scalastyle:off
object AvroRecursiveSchemaTest extends AvroOperationDsl {
  def main(args: Array[String]): Unit = {
    println(schemaOf[TestClass].format())
    val schema1 =
      SchemaBuilder.record("R")
        .fields
        .name("b").`type`(Schema.create(Schema.Type.INT)).withDefault(1)
        .name("r").`type`("R").withDefault(null).endRecord
    println(schema1)

    val record = new GenericData.Record(schema1)
    record.put(
      "r",
      new GenericData.Record(schema1)
    )

    println(record)


    import scala.collection.JavaConverters._
    val schema2 = Schema.createRecord(
      "a",
      null,
      "a",
      false,
      Seq().asJava
    )
    println(schema2)

    val avroOp1 = avroOperationOf[Recursive]()
    println(avroOp1.avroSchema)

    val avroOp = avroOperationOf[TestClass]()
    val schema = avroOp.avroSchema

    val buffer = new ByteArrayOutputStream(1000)
    val avroWriter = avroOp.newWriter(buffer)
    avroWriter.write(TestClass(1))
    println(new String(buffer.toByteArray))

    // println(schema)
    // println(schema)
  }
}
