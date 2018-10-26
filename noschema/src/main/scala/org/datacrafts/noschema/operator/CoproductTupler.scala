package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{NoSchemaCoproduct, Operation}
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.operator.CoproductOperator.CoproductInfo

class CoproductTupler[T](
  override val operation: Operation[T],
  override val coproduct: NoSchemaCoproduct[T]
) extends CoproductOperator [T, (String, Any)]{

  override def matchInputWithCoproductElement(input: Any,
    coproductElement: CoproductElement[_]
  ): Option[Any] = {
    input match {
      case (key, value) =>
        if (key == coproductElement.noSchema.scalaType.fullName) Some(value) else None
      case _ => throw new Exception(
        s"input ${input} ${input.getClass} does not match key value pair")
    }
  }

  override def coproductInfoToOutput(coproductInfo: CoproductInfo): (String, Any) = {
    (coproductInfo.coproductElement.noSchema.scalaType.fullName, coproductInfo.value)
  }

}
