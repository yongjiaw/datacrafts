package org.datacrafts.noschema.avro

import org.datacrafts.noschema.{Context, NoSchema, NoSchemaDsl}
import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectionRule

trait AvroOperationDsl extends NoSchemaDsl {

  def deprecatedAvroOperationOf[T: NoSchema](
    avroRule: AvroRule = AvroRule.Default): AvroOperation[T] = {
    new AvroOperation[T](Context.root(schemaOf[T]), avroRule, None)
  }

  def avroOperationOf[T: scala.reflect.runtime.universe.TypeTag](
    avroRule: AvroRule = AvroRule.Default,
    reflectionRule: ReflectionRule = new ReflectionRule {}
  ): AvroOperation[T] = {
    new AvroOperation[T](
      Context.root(
        reflectedSchemaOf[T](reflectionRule)),
      avroRule,
      None)
  }

}

object AvroOperationDsl extends AvroOperationDsl
