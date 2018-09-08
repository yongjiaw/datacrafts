package org.datacrafts.noschema.operator

import org.datacrafts.noschema.Operation

class AnyOperator(override val operation: Operation[Any]) extends Operation.Operator[Any] {

  override protected def marshalNoneNull(input: Any): Any = input

  override protected def unmarshalNoneNull(input: Any): Any = input
}
