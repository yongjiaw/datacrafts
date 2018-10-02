package org.datacrafts.noschema.json

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Context, Operation, ShapelessCoproduct, ShapelessProduct}
import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.json.JsonOperation.JsonConfig
import org.datacrafts.noschema.operator.{ShapelessCoproductOperator, ShapelessProductMapper}
import org.datacrafts.noschema.rule.DefaultRule

class JsonOperation[T](
  context: Context[T],
  jsonConfig: JsonConfig,
  coproductOperatorGetter: JsonOperation.CoproductOperatorGetter
) extends Operation[T](
  context = context,
  rule = new DefaultRule with Slf4jLogging.Default {
    override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {
      operation.context.noSchema match {

        case shapeless: ShapelessProduct[V, _] =>
          new ShapelessProductMapper(
            operation, shapeless,
            allowUnknownField = jsonConfig.allowUnknownField,
            allowAbsence = jsonConfig.allowAbsence,
            includeNull = jsonConfig.includeNull
          )

        case shapeless: ShapelessCoproduct[V, _] =>
          coproductOperatorGetter.getOperator(shapeless, operation)

        case _ => super.getOperator(operation)
      }

    }
  }
) {

  def toJson(input: T,
    pretty: Boolean = false
  ): String = {
    if (pretty) {
      JsonOperation.objectMapper.writerWithDefaultPrettyPrinter()
        .writeValueAsString(unmarshal(input))
    }
    else {
      JsonOperation.objectMapper.writeValueAsString(unmarshal(input))
    }

  }

  def fromJson(input: String): T = {
    marshal(JsonOperation.objectMapper.readValue[Map[String, Any]](input))
  }

}

object JsonOperation {

  case class JsonConfig(
    allowUnknownField: Boolean = false,
    allowAbsence: Boolean = true,
    includeNull: Boolean = false
  )

  val objectMapper = new ObjectMapper() with ScalaObjectMapper
  objectMapper.registerModule(DefaultScalaModule)

  // there are many different ways to handle coproduct serialization
  // using a format that makes the JSON output more self-explained
  // make the properties configurable
  trait CoproductOperatorGetter {
    def getOperator[T](
      shapelessCoproduct: ShapelessCoproduct[T, _],
      operation: Operation[T]
    ): Operation.Operator[T] = {
      new JsonCoproductOperator[T](shapelessCoproduct, operation)
    }
  }
  class JsonCoproductOperator[T](
    override val shapeless: ShapelessCoproduct[T, _],
    override val operation: Operation[T]
  ) extends ShapelessCoproductOperator[T, Map[String, Any]] {

    // this determines the value of the coproduct key
    // must be consistent during seriaization and deserialization
    protected def getCoproductType(coproductElement: CoproductElement[_]): String = {
      coproductElement.noSchema.scalaType.fullName
    }

    final override def matchInputWithCoproductElement(
      input: Any,
      coproductElement: CoproductElement[_]
    ): Option[Any] = input match {

      case map: Map[_, _] =>
        map.asInstanceOf[Map[String, _]].get(getCoproductType(coproductElement))

      case _ => throw new Exception(
        s"input type ${input.getClass} is not Map: $input")
    }

    final override protected def coproductInfoToOutput(
      coproductInfo: ShapelessCoproductOperator.CoproductInfo
    ): Map[String, Any] = {
      Map(
        getCoproductType(coproductInfo.coproductElement) -> coproductInfo.value
      )
    }
  }

}
