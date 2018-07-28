package org.datacrafts.noschema.avro

import java.io.OutputStream

import scala.collection.JavaConverters._

import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.apache.avro.file.DataFileWriter
import org.apache.avro.generic.GenericDatumWriter
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Context, Operation, ShapelessCoproduct}
import org.datacrafts.noschema.Context.LocalContext
import org.datacrafts.noschema.NoSchema.ScalaType
import org.datacrafts.noschema.avro.AvroRule.SchemaWrapper

class AvroOperation[T](
  context: Context[T],
  avroRule: AvroRule,
  val parentOp: Option[AvroOperation[_]])
  extends Operation[T](context, avroRule) with Slf4jLogging.Default {

  override lazy val operator: Operation.Operator[T] = avroRule.getAvroOperator(this)

  override lazy val dependencyOperationMap: Map[Context.LocalContext[_], AvroOperation[_]] =
    context.noSchema.dependencies.map {
      dependency =>
        dependency -> new AvroOperation(context.dependencyContext(dependency), avroRule, Some(this))
    }.toMap

  override def dependencyOperation[D](dependency: LocalContext[D]): AvroOperation[D] = {
    super.dependencyOperation(dependency).asInstanceOf[AvroOperation[D]]
  }

  // use lazy val to cache all the results derived by rules for efficient re-access
  // the same check will be performed both for schema and for operating with the data
  // the rule must be consistent (immutable)
  lazy val avroSchema: Schema = {
    val schema = avroRule.getSchema(this)
    schemaWrapper match {
      case Some(SchemaWrapper(name, nameSpace, wrapperField)) =>
      Schema.createRecord(
        name,
        avroRule.getSchemaDoc(this),
        nameSpace,
        false,
        Seq(
          new Field(
            wrapperField,
            schema,
            avroRule.getFieldDoc(this),
            null.asInstanceOf[Any] // scalastyle:ignore
          )
        ).asJava
      )
      case None => schema
    }
  }

  lazy val isNullable: Boolean = avroSchema.getType == Schema.Type.UNION &&
    avroSchema.getTypes.asScala.exists(_.getType == Schema.Type.NULL)

  lazy val isUnion: Boolean = {
    context.noSchema match {
      case shapeless: ShapelessCoproduct[_, _] => avroRule.isUnion(shapeless)
      case _ => false
    }
  }

  lazy val isEnum: Boolean = {
    context.noSchema match {
      case shapeless: ShapelessCoproduct[_, _] => avroRule.isEnum(shapeless)
      case _ => false
    }
  }

  lazy val schemaWrapper: Option[SchemaWrapper] = avroRule.getSchemaWrapper(this)

  def toAvro(input: T): Any = {
    val result = operator.unmarshal(input)
    logDebug(s"converted $input to $result")
    result
  }

  def scalaType: ScalaType[T] = context.noSchema.scalaType

  def anyToAvro(input: Any): Any = {
    scalaType.matchInput(input) match {
      case Some(t) => toAvro(t)
      case None =>
        throw new Exception(
          s"input ${input.getClass.getCanonicalName} is not instance of ${scalaType}: $input")
    }
  }

  def newWriter(
    outputStream: OutputStream,
    setWriterProperties: (DataFileWriter[Any]) => Unit = (dataFileWriter) => {}
  ): AvroWriter = {
    new AvroWriter(outputStream, setWriterProperties)
  }

  final class AvroWriter(
    outputStream: OutputStream, setProperties: (DataFileWriter[Any]) => Unit) {

    private lazy val _dataFileWriter = {
      val writer = new GenericDatumWriter[Any](avroSchema)
      val dataFileWriter = new DataFileWriter[Any](writer)
      // these properties can be set by caller
      // dataFileWriter.setCodec(CodecFactory.snappyCodec())
      // dataFileWriter.setFlushOnEveryBlock(false)
      setProperties(dataFileWriter)
      // add schema at beginning of the outputStream
      dataFileWriter.create(avroSchema, outputStream)
      dataFileWriter
    }

    def write(input: T): Unit = {
      _dataFileWriter.append(toAvro(input))
    }

    def writeAny(input: Any): Unit = {
      _dataFileWriter.append(anyToAvro(input))
    }

    def flush(): Unit = {
      _dataFileWriter.flush()
    }

    def close(): Unit = {
      _dataFileWriter.close()
    }

  }
}
