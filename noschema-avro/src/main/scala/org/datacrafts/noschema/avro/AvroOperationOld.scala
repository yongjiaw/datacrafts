package org.datacrafts.noschema.avro

import java.io.OutputStream

import org.apache.avro.Schema
import org.apache.avro.file.DataFileWriter
import org.apache.avro.generic.GenericDatumWriter
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{NoSchemaDsl, Operation}
import org.datacrafts.noschema.Context.{CoproductElement, LocalContext, MemberVariable}
import org.datacrafts.noschema.avro.AvroOperationOld.SchemaInfo
import org.datacrafts.noschema.NoSchema.ScalaType

class AvroOperationOld[T](val operation: Operation[T], val avroRule: DefaultAvroRule)
  extends Slf4jLogging.Default {

  lazy val schemaInfo: SchemaInfo = avroRule.getAvroSchema(operation)
  lazy val avroSchema: Schema = schemaInfo.avroSchema

  def toAvro(input: T): Any = {
    val result = operation.operator.unmarshal(input)
    logDebug(s"converted $input to $result")
    result
  }

  def scalaType: ScalaType[T] = operation.context.noSchema.scalaType

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

object AvroOperationOld {
  case class SchemaInfo(
    avroSchema: Schema,
    unionMemberWrappedSchema: Map[CoproductElement[_], Option[Schema]] = Map.empty,
    recordMemberNullable: Map[MemberVariable[_], Boolean] = Map.empty
  ) {
    val isEnum = avroSchema.getType == Schema.Type.ENUM
    val isUnion = avroSchema.getType == Schema.Type.UNION

    def getWrappedSchema(coproductElement: CoproductElement[_]): Option[Schema] = {
      unionMemberWrappedSchema.get(coproductElement).getOrElse(
        throw new Exception(
          s"calling with unrecognized member ${coproductElement}, " +
            s"not found in ${unionMemberWrappedSchema.keySet}. " +
            s"This should not happen under intended usage"
        )
      )
    }
    def memberIsNullable(member: MemberVariable[_]): Boolean = {
      recordMemberNullable.get(member).getOrElse(
        throw new Exception(
          s"calling with unrecognized member ${member}, " +
            s"not found in ${recordMemberNullable.keySet}. " +
            s"This should not happen under intended usage"
        )
      )
    }
  }
}
