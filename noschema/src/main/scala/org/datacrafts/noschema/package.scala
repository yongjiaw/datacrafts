package org.datacrafts

import org.datacrafts.noschema.implicits.{ShapelessCoproduct, ShapelessProduct}

package object noschema extends Primitive.Instances with Container.Instances
  with ShapelessProduct.Instances with ShapelessCoproduct.Instances {
  val schemaClassFilter = collection.mutable.Set.empty[String]

  implicit case object AnyType extends NoSchema[Any](
    category = NoSchema.Category.Any,
    nullable = true
  )

}
