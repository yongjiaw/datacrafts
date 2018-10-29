package org.datacrafts.noschema

class Container[T: NoSchema.ScalaType, C: NoSchema.ScalaType](
  category: NoSchema.Category.Value,
  val element: Context.ContainerElement[T],
  nullable: Boolean = true
) extends NoSchema[C](category = category, nullable = nullable) {
  type Elem = T
}
