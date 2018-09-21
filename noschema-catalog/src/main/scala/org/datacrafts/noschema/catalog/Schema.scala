package org.datacrafts.noschema.catalog

import org.datacrafts.noschema.{AnyType, Context, NoSchema, NoSchemaDsl, ShapelessCoproduct, ShapelessProduct}

sealed trait Schema {
  def schemaType: Schema.Type
}

object Schema extends NoSchemaDsl {

  case class Type(id: String,
    metadata: Map[String, String]
  )

  case class Product(fields: Map[String, Schema],
    schemaType: Type
  ) extends Schema

  case class CoProduct(members: Seq[Schema],
    schemaType: Type
  ) extends Schema

  case class Container(element: Schema,
    schemaType: Type
  ) extends Schema

  case class Primitive(schemaType: Type) extends Schema

  case class Reference(typeId: String) extends Schema {
    override val schemaType: Type = Type(id = typeId, metadata = Map("id" -> typeId))
  }

  def of[T: NoSchema]: Schema = {
    implicitly[NoSchema[T]].getSchema(Set.empty)
  }

  implicit class NoSchemaWrapper(val noSchema: NoSchema[_]) {

    def getSchema(visitedSchema: Set[String]): Schema = {
      noSchema match {
        case primitive: org.datacrafts.noschema.Primitive[_] =>
          Primitive(
            Type(
              id = "primitive",
              metadata = Map("scalaType" -> primitive.scalaType.fullName)
            )
          )
        case product: ShapelessProduct[_, _] =>
          Product(
            fields =
              if (visitedSchema.contains(product.scalaType.uniqueKey.toString)) {
                Map.empty
              } else {

                product.dependencies.map {
                  member =>
                    member.symbol.toString() -> member.noSchema.getSchema(
                      visitedSchema + product.scalaType.uniqueKey.toString
                    )
                }.toMap
              },
            schemaType =
              Type(
                id = "product",
                metadata = Map("scalaType" -> product.scalaType.uniqueKey.toString)
              )
          )
        case coproduct: ShapelessCoproduct[_, _] =>
          CoProduct(
            members = coproduct.dependencies.map {
              member => member.noSchema.getSchema(visitedSchema)
            },
            schemaType =
              Type(
                id = "coproduct",
                metadata = Map("scalaType" -> coproduct.scalaType.fullName)
              )
          )
        case container: org.datacrafts.noschema.Container[_, _] =>
          Container(
            element = container.element.noSchema.getSchema(visitedSchema),
            schemaType =
              Type(
                id = "container",
                metadata = Map("scalaType" -> container.scalaType.fullName)
              )
          )
        case AnyType =>
          Primitive(
            Type(
              id = "any",
              metadata = Map("scalaType" -> AnyType.scalaType.fullName)
            )
          )
        case _ =>
          throw new Exception(s"cannot convert to catalog schema: ${noSchema.format()}")
      }
    }
  }

}
