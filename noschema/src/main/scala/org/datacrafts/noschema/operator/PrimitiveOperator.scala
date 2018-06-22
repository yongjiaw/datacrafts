package org.datacrafts.noschema.operator

import org.datacrafts.noschema.Operation

class PrimitiveOperator[T](
  override val operation: Operation[T]
) extends Operation.Operator[T]{

  protected override def marshalNoneNull(input: Any): T = input.asInstanceOf[T]

  protected override def unmarshalNoneNull(input: T): Any = input
}
