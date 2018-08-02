package org.datacrafts.noschema.json

import org.datacrafts.noschema.{Context, NoSchema, NoSchemaDsl}
import org.datacrafts.noschema.json.JsonOperation.JsonConfig

trait JsonOperationDsl extends NoSchemaDsl {
  def jsonOperationOf[T: NoSchema](jsonConfig: JsonConfig = JsonConfig()): JsonOperation[T] = {
    new JsonOperation[T](Context.root(schemaOf[T]), jsonConfig = jsonConfig)
  }
}

object JsonOperationDsl extends JsonOperationDsl
