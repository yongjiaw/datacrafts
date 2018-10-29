package org.datacrafts.noschema.avro

import org.datacrafts.noschema.{Context, NoSchema, NoSchemaDsl}
import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectionRule

trait AvroOperationDsl {

  Self: NoSchemaDsl =>

  def avroOperationof[T](
    noSchema: NoSchema[T],
    avroRule: AvroRule
  ): AvroOperation[T] = new AvroOperation[T](Context.root(noSchema), avroRule, None)

  def avroOperationByShapeless[T: NoSchema](
    avroRule: AvroRule = AvroRule.Default): AvroOperation[T] = {
    avroOperationof(schemaByShapeless[T], avroRule)
  }

  def avroOperationByReflection[T: scala.reflect.runtime.universe.TypeTag](
    avroRule: AvroRule = AvroRule.Default,
    schemaReflectionRule: ReflectionRule = new ReflectionRule {}
  ): AvroOperation[T] = {
    avroOperationof(schemaByReflection[T](schemaReflectionRule), avroRule)
  }

}

object AvroOperationDsl extends AvroOperationDsl with NoSchemaDsl
