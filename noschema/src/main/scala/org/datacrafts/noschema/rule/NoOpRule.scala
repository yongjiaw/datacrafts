package org.datacrafts.noschema.rule

import org.datacrafts.noschema.Operation

object NoOpRule extends Operation.Rule {
  override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {
    new Operation.Operator[V]{

      override def toString: String = "NoOp"

      override def operation: Operation[V] = throw new Exception(s"should not call this")

      override protected def marshalNoneNull(input: Any): V =
        throw new Exception(s"should not call this")

      override protected def unmarshalNoneNull(input: V): Any =
        throw new Exception(s"should not call this")
    }
  }
}
