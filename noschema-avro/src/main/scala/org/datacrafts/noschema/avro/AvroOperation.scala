package org.datacrafts.noschema.avro

import java.io.OutputStream

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.apache.avro.Schema
import org.apache.avro.file.DataFileWriter
import org.apache.avro.generic.GenericDatumWriter
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{NoSchemaDsl, Operation}

class AvroOperation[T](val operation: Operation[T], avroRule: DefaultAvroRule)
  extends Slf4jLogging.Default {

  lazy val avroSchema: Schema = avroRule.getAvroSchema(operation)

  def toAvro(input: T): Any = {
    val result = operation.operator.unmarshal(input)
    logDebug(s"converted $input to $result")
    result
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

    def flush(): Unit = {
      _dataFileWriter.flush()
    }

    def close(): Unit = {
      _dataFileWriter.close()
    }

  }

}
