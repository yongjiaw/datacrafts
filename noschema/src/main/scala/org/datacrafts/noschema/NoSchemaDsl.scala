package org.datacrafts.noschema

import org.datacrafts.noschema.Operation.SchemaFormatter
import org.datacrafts.noschema.rule.DefaultRule
import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectionRule

trait NoSchemaDsl {

  // this method use compile time implicit resolution which was setup with shapeless
  def schemaOf[T: NoSchema]: NoSchema[T] = {
    implicitly[NoSchema[T]]
  }

  import scala.reflect.runtime.universe.TypeTag
  // this method uses runtime reflection with TypeTag
  def reflectedSchemaOf[T: TypeTag](
    rule: ReflectionRule = new ReflectionRule {}
  ): NoSchema[T] = {
    rule.reflect(implicitly[TypeTag[T]].tpe).lazySchema.asInstanceOf[NoSchema[T]]
  }

  implicit class NoSchemaConverter[T](noschema: NoSchema[T]) {
    def operation(rule: Operation.Rule = DefaultRule): Operation[T] = {
      new Operation[T](Context.root(noschema), rule)
    }

    def format(formatter: SchemaFormatter = new SchemaFormatter): String = {
      operation().format(formatter)
    }
  }

  implicit class OperationRuleConverter(rule: Operation.Rule) {
    def withSchema[T: NoSchema]: Operation[T] = {
      new Operation[T](Context.root(schemaOf[T]), rule)
    }
  }
}
