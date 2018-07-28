package org.datacrafts.noschema.avro

import org.datacrafts.noschema.{Context, NoSchema, NoSchemaDsl}

trait AvroOperationDsl extends NoSchemaDsl {

  def avroOperationOf[T: NoSchema](
    avroRule: AvroRule = AvroRule.Default): AvroOperation[T] = {
    new AvroOperation[T](
      Context.root(schemaOf[T]),
      avroRule,
      None
    )
  }

}

