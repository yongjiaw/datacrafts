package org.datacrafts.noschema.avro

import java.io.ByteArrayOutputStream

import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.apache.avro.generic.GenericData.EnumSymbol
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.ScroogeSupport
import org.datacrafts.scrooge.shapes.{NestedUnion, StructExample, UnionExample}
import org.scalatest.FlatSpec

// scalastyle:off
class AvroSchemaTest extends FlatSpec with AvroOperationDsl with ScroogeSupport {
  "nested union" should "be wrapped" in {
    import scala.collection.JavaConverters._
    val schema = avroOperationOf[NestedUnion]().avroSchema
    println(schema)
    val expectedSchema =
      Schema.createUnion(
        Schema.createRecord( // this is the wrapper record for the union
          "A", null,
          "org.datacrafts.scrooge.shapes.NestedUnion", false,
          List(
            new Field(
              "UnionExample",
              avroOperationOf[UnionExample]().avroSchema,
              null,
              null)).asJava
        ),
        // order of union matters
        Schema.create(Schema.Type.INT),
        Schema.create(Schema.Type.STRING)
      )

    assert(schema == expectedSchema)
  }

  def assertRoundTrip[T](avroOp: AvroOperation[T], input: T): Unit = {
    assert(avroOp.fromAvro(avroOp.toAvro(input)) == input)
  }

  "Enum marshal/unmarshal" should "be successful" in {
    val avroOp = avroOperationOf[TestEnum]()
    assert(avroOp.toAvro(AE) == new EnumSymbol(avroOp.avroSchema, "AE"))
    assert(avroOp.toAvro(BE) == new EnumSymbol(avroOp.avroSchema, "BE"))
    assertRoundTrip(avroOp, AE)
    assertRoundTrip(avroOp, BE)

  }

  "Union marshal/unmarshal" should "be successful" in {
    val avroOp = avroOperationOf[TestUnion]()
    assertRoundTrip(avroOp, AU(1))
    assertRoundTrip(avroOp, BU("a"))
  }

  "NestedUnion marshal/unmarshal" should "be successful" in {
    val avroOp = avroOperationOf[NestedUnion]()
    println(avroOp.format())
    assertRoundTrip(avroOp, NestedUnion.A(UnionExample.A(StructExample(foo = "bar"))))
    assertRoundTrip(avroOp, NestedUnion.A(UnionExample.B(2)))
    assertRoundTrip(avroOp, NestedUnion.A(UnionExample.C("c")))
    assertRoundTrip(avroOp, NestedUnion.B(1))
    assertRoundTrip(avroOp, NestedUnion.C("1"))
  }

  "case class and thrift marshal/unmarshal" should "be successful" in {

    val op = schemaOf[Option[Option[Option[Double]]]].operation()
    println(op.operator.marshal(null)) // Some(Some(None))) instead of None

    val avroOp = avroOperationOf[Test2]()
    val value = Test2(1.1, Test(12))
    val avro = avroOp.toAvro(value)
    println(avro)
    val expectedString = """{"v1": 1.1, "v10": {"UnionExample": {"bar": null, "foo": "bar"}}, "v2": {"v3": 12, "v4": 0.1}, "v3": {"v1": 1}, "v4": "BE", "v5": {"x": {"v2": "a"}}, "v6": [1.5], "v7": {"bytes": ""}, "v8": [["1", "2"], [], ["3"]], "v9": null}"""
    assert(avro.toString == expectedString)

    assert(avroOp.fromAvro(avro) == value)

    // write to avro file should be successful too
    val buffer = new ByteArrayOutputStream(1000)
    val avroWriter = avroOp.newWriter(buffer)
    avroWriter.write(value)
  }
}

object AvroScroogeRule extends AvroRule with Slf4jLogging.Default {

}

case class Test(v3: Int, v4: Option[Option[Option[Double]]] = Some(Some(Some(0.1))))
case class Test2(v1: Double, v2: Test,
  v3: TestUnion = AU(1),
  v4: TestEnum = BE,
  v5: Map[String, TestUnion] = Map("x" -> BU("a")),
  v6: Seq[Double] = Seq(1.5),
  v7: Array[Byte] = Array.emptyByteArray,
  v8: Seq[Seq[String]] = Seq(Seq("1", "2"), Seq.empty, Seq("3")),
  v9: Option[TestEnum] = None,
  v10: Option[NestedUnion] = Some(NestedUnion.A(a = UnionExample.A(StructExample(foo = "bar"))))
)

sealed trait TestUnion
case class AU(v1: Int) extends TestUnion
case class BU(v2: String) extends TestUnion

sealed trait TestEnum
case object AE extends TestEnum
case object BE extends TestEnum
