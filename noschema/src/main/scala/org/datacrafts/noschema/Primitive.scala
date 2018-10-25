package org.datacrafts.noschema

class Primitive[T: NoSchema.ScalaType](
  val refinedType: Primitive.Type.Value,
  nullable: Boolean = false)
  extends NoSchema[T](
    category = NoSchema.Category.Primitive,
    nullable = nullable
  ) {
}

object Primitive {

  object Type extends Enumeration {
    val Int, Short, Long, Boolean, Bytes, String, Double, Float = Value
  }

}
