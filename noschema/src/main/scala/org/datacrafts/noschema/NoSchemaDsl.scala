package org.datacrafts.noschema

import org.datacrafts.noschema.Operation.{DefaultFormatter, SchemaFormatter}
import org.datacrafts.noschema.rule.DefaultRule

trait NoSchemaDsl {

  def schemaOf[T: NoSchema]: NoSchema[T] = {
    implicitly[NoSchema[T]]
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
