package org.datacrafts.noschema.avro

import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.datacrafts.noschema.{NoSchema, Operation, Primitive, ShapelessCoproduct, ShapelessProduct}
import org.datacrafts.noschema.avro.AvroOperationOld.SchemaInfo
import org.datacrafts.noschema.Context.MemberVariable

trait AvroSchema {
  def schema: Schema

  import scala.collection.JavaConverters._
  lazy val isNullable: Boolean = schema.getType == Schema.Type.UNION &&
    schema.getTypes.asScala.exists(_.getType == Schema.Type.NULL)
}

object AvroSchema {

  val NameSpacePattern = """(.+)\.(.+)""".r
  import scala.collection.JavaConverters._
  import scala.reflect.runtime.universe.typeOf

  class PrimitiveSchema(primitive: Primitive[_]) extends AvroSchema {

    override lazy val schema: Schema = {
        primitive.scalaType.typeTag.tpe match {
          case t if t <:< typeOf[Short] => Schema.create(Schema.Type.INT)
          case t if t <:< typeOf[Int] => Schema.create(Schema.Type.INT)
          case t if t <:< typeOf[Long] => Schema.create(Schema.Type.LONG)
          case t if t <:< typeOf[Double] => Schema.create(Schema.Type.DOUBLE)
          case t if t <:< typeOf[Float] => Schema.create(Schema.Type.FLOAT)
          case t if t <:< typeOf[Boolean] => Schema.create(Schema.Type.BOOLEAN)
          case t if t <:< typeOf[String] => Schema.create(Schema.Type.STRING)
          case t if t <:< typeOf[Array[Byte]] => Schema.create(Schema.Type.BYTES)
          case _ => throw new Exception(s"no primitive avro schema for ${primitive}")
        }
    }
  }

  class ProductRecordSchema(
    name: String,
    namespace: String,
    dependencySchema: Map[String, AvroSchema],
    doc: Option[String] = None) extends AvroSchema {

    protected def getFieldDoc(fieldName: String): String = null

    override lazy val schema: Schema = {
      Schema.createRecord(
        name,
        doc.getOrElse(null),
        namespace,
        false,
        dependencySchema.map {
          case (depName, schema) =>
            new Field(
              depName,
              schema.schema,
              getFieldDoc(depName),
              // no default value needed for avro schema,
              // since there won't be missing values
              // when generating avro record from scala class.
              // default in avro schema plays the same role as the default in operator,
              // which is needed when marshaling from avro input to scala class
              null.asInstanceOf[Any] // scalastyle:ignore
            )
        }.toSeq.sortBy(_.name()).asJava
      )
    }
  }


  // nested union is possible for thrift: it's actually the artifact of the thrift shapeless library
  // even the original thrift classes always have a case class wrapper,
  // the warpper was removed by the shapeless library
  // we may also want to wrap array of array, which is not accepted by BigQuery
  class WrappedRecordSchema()

  class ShapelessCoproductUnionSchema(
    shapeless: ShapelessCoproduct[_, _],
    operation: Operation[_],
    rule: AvroRuleOld) extends AvroSchema {
    override def schema: Schema = ???
  }

  class ShapelessCoproductEnumSchema(
    shapeless: ShapelessCoproduct[_, _],
    operation: Operation[_],
    rule: AvroRuleOld) extends AvroSchema {
    override def schema: Schema = ???
  }

  class OptionSchema(elementSchema: AvroSchema) extends AvroSchema {
    override lazy val schema: Schema = {
      // already nullable union
      if (elementSchema.isNullable) {
        elementSchema.schema
      } else if ( // union but not nullable
        elementSchema.schema.getType == Schema.Type.UNION
      ) {
        // add NULL type to the union
        Schema.createUnion(
          (elementSchema.schema.getTypes.asScala :+ Schema.create(Schema.Type.NULL)).asJava
        )
      } else { // not union can be wrapped
        Schema.createUnion(
          elementSchema.schema,
          Schema.create(Schema.Type.NULL)
        )
      }
    }
  }

  class MapSchema(elementSchema: AvroSchema) extends AvroSchema {
    override lazy val schema: Schema = Schema.createMap(elementSchema.schema)
  }


  class ArraySchema(elementSchema: AvroSchema) extends AvroSchema {
    override lazy val schema: Schema = Schema.createArray(elementSchema.schema)
  }


}
