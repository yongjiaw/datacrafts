package org.datacrafts.noschema

import org.datacrafts.noschema.Operation.DefaultFormatter
import org.datacrafts.noschema.rule.DefaultRule

trait NoSchemaDsl {

  def schemaOf[T: NoSchema]: NoSchema[T] = {
    implicitly[NoSchema[T]]
  }

  implicit class NoSchemaConverter[T](noschema: NoSchema[T]) {
    def operation(rule: Operation.Rule = DefaultRule): Operation[T] = {
      new Operation[T](Context.root(noschema), rule)
    }

    def format(): String = {
      operation().format(
        new DefaultFormatter(false)
      )
    }
  }

  implicit class OperationRuleConverter(rule: Operation.Rule) {
    def withSchema[T: NoSchema]: Operation[T] = {
      new Operation[T](Context.root(implicitly[NoSchema[T]]), rule)
    }
  }
}
