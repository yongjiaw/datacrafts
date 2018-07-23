package org.datacrafts.noschema.avro

import org.datacrafts.noschema.{NoSchema, NoSchemaDsl}

trait AvroOperationDsl extends NoSchemaDsl {

  def avroOperationOf[T: NoSchema](
    avroRule: DefaultAvroRule = DefaultAvroRule): AvroOperation[T] = {
    new AvroOperation[T](
      schemaOf[T].operation(avroRule),
      avroRule
    )
  }

}

