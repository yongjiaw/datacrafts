package org.datacrafts.noschema

import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectionRule
import org.datacrafts.noschema.reflection.ScroogeReflectionRule

trait ThriftSchemaDsl extends NoSchemaDsl with ScroogeSupport {
  import scala.reflect.runtime.universe.TypeTag
  // this method uses runtime reflection with TypeTag
  override def schemaByReflection[T: TypeTag](
    rule: ReflectionRule = ScroogeReflectionRule
  ): NoSchema[T] = {
    rule.reflect(implicitly[TypeTag[T]].tpe).asInstanceOf[NoSchema[T]]
  }
}
