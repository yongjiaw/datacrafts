package org.datacrafts.noschema.json

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Context, NoSchema, NoSchemaDsl, Operation, Primitive, ShapelessProduct}
import org.datacrafts.noschema.json.JsonOperation.JsonConfig
import org.datacrafts.noschema.operator.{PrimitiveOperator, ShapelessProductMapper}
import org.datacrafts.noschema.rule.DefaultRule

class JsonOperation[T](
  context: Context[T],
  jsonConfig: JsonConfig
) extends Operation[T] (
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
        case _ => super.getOperator(operation)
      }

    }
  }
) {

  def toJson(input: T, pretty: Boolean = false): String = {
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
    includeNull: Boolean = true
  )

  val objectMapper = new ObjectMapper() with ScalaObjectMapper
  objectMapper.registerModule(DefaultScalaModule)
}
