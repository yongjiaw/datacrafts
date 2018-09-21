package org.datacrafts.noschema.catalog

import org.datacrafts.noschema.json.{JsonOperation, JsonOperationDsl}

case class Test(a: String)

object SchemaTest extends JsonOperationDsl {

  def main(args: Array[String]): Unit = {

    // println(Schema.of[Schema.Product])
    println(
      jsonOperationOf[Schema]().toJson(
        Schema.of[Test], true
      )
    )

    println(
      JsonOperation.objectMapper.writerWithDefaultPrettyPrinter()
        .writeValueAsString(
          Schema.of[Schema]
        )
    )


    println(schemaOf[Schema].format())
    // println(Schema.of[Schema])
/*
    println(Schema.of[TestClass])

    println(
    jsonOperationOf[Schema]().toJson(
      Schema.of[TestClass],
      true
    )
    )
*/
  }
}
