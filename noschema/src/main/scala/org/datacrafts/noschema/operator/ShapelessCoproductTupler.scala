package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{Operation, ShapelessCoproduct}
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.ShapelessCoproduct.TypeValueExtractor
import org.datacrafts.noschema.operator.ShapelessCoproductOperator.{CoproductBuilder, CoproductInfo}

class ShapelessCoproductTupler[T](
  override val operation: Operation[T],
  override val shapeless: ShapelessCoproduct[T, _]
) extends ShapelessCoproductOperator [T, (String, Any)]{

  override def matchInputWithCoproductElement(input: Any,
    coproductElement: CoproductElement[_]
  ): Option[Any] = {
    input match {
      case (key, value) =>
        if (s"$key" == coproductElement.noSchema.scalaType.fullName) Some(value) else None
      case _ => throw new Exception(
        s"input ${input} ${input.getClass} does not match key value pair")
    }
  }

  override def coproductInfoToOutput(coproductInfo: CoproductInfo): (String, Any) = {
    (coproductInfo.coproductElement.noSchema.scalaType.fullName, coproductInfo.value)
  }

}
