package org.datacrafts.noschema.catalog

import org.datacrafts.noschema.{Context, NoSchemaDsl, Operation, ShapelessCoproduct}
import org.datacrafts.noschema.NoSchemaTest.TestClass
import org.datacrafts.noschema.catalog.NoSchemaExternalizable.SchemaWithReference
import org.datacrafts.noschema.json.{JsonOperation, JsonOperationDsl}
import org.datacrafts.noschema.json.JsonOperation.{JsonConfig, JsonCoproductOperator}


object NoSchemaCatalogTest extends JsonOperationDsl {
  def main(args: Array[String]): Unit = {
    val schema = schemaOf[TestClass]
    println(schema.format())
    val serializedSchema = new ScalaSchemaExternalizer(Context.Root(schema), None).getExternalizedSchema()

    // the AST of the serialized schema
    println(schemaOf[SchemaWithReference].format())

    val jsonOp = jsonOperationOf[SchemaWithReference](
      jsonConfig = JsonConfig(includeNull = false),
      coproductOperatorGetter = new JsonOperation.CoproductOperatorGetter {
        override def getOperator[T](shapelessCoproduct: ShapelessCoproduct[T, _],
          operation: Operation[T]
        ): JsonOperation.JsonCoproductOperator[T] =
          new JsonCoproductOperator[T](shapelessCoproduct, operation) {
            override protected def getCoproductType(
              coproductElement: Context.CoproductElement[_]): String = {
              // for schema serialization, shortName is sufficient
              coproductElement.noSchema.scalaType.shortName
            }
          }
      }
    )

    val json = jsonOp.toJson(serializedSchema, true)

    println(json)

    println(jsonOp.fromJson(json) == serializedSchema)

  }
}
