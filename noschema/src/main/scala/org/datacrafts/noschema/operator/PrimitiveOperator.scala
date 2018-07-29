package org.datacrafts.noschema.operator

import org.datacrafts.noschema.Operation

class PrimitiveOperator[T](
  override val operation: Operation[T]
) extends Operation.Operator[T]{

  protected override def marshalNoneNull(input: Any): T = {
    operation.context.noSchema.scalaType.matchInput(input) match {
      case Some(value) => value
      case None => throw new Exception(
        s"input ${input}, class is ${input.getClass} " +
          s"does not match ${operation.context.noSchema.scalaType}")
    }
  }

  protected override def unmarshalNoneNull(input: T): Any = input
}
