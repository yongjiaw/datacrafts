package org.datacrafts.noschema.avro

import java.io.ByteArrayOutputStream

object AvroSchemaTest extends AvroOperationDsl {
  def main(args: Array[String]): Unit = {

    val avroOp = avroOperationOf[Test2]()

    println(avroOp.operation.context.noSchema.format())
    println(avroOp.avroSchema)
    println(avroOp.toAvro(Test2(1.1, Test(12))))

    val buffer = new ByteArrayOutputStream(1000)
    val avroWriter = avroOp.newWriter(buffer)
    avroWriter.write(Test2(1.1, Test(1)))
    println(new String(buffer.toByteArray))

    val buffer2 = new ByteArrayOutputStream(1000)
    val avroWriter2 = avroOperationOf[Int]().newWriter(buffer2)
    avroWriter2.write(1)
    println(new String(buffer2.toByteArray))

  }
}

case class Test(v3: Int, v4: Option[Option[Option[Double]]] = None)
case class Test2(v1: Double, v2: Test,
  v3: TestUnion = AU(1),
  v4: TestEnum = BE,
  v5: Map[String, TestUnion] = Map("x" -> BU("a")),
  v6: Seq[Double] = Seq(1.5),
  v7: Array[Byte] = Array(1)
)

sealed trait TestUnion
case class AU(v1: Int) extends TestUnion
case class BU(v1: String) extends TestUnion

sealed trait TestEnum
case object AE extends TestEnum
case object BE extends TestEnum
