package org.datacrafts

package object noschema extends Primitive.Instances with Container.Instances
  with ShapelessProduct.Instances with ShapelessCoproduct.Instances {
  val schemaClassFilter = collection.mutable.Set.empty[String]
}
