package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.noschema.{Container, Context, NoSchema}
import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectedScalaType

object ReflectedContainer {

  class RfOption(
    runtimeType: ru.Type,
    element: Context.ContainerElement[Any]
  ) extends Container[Any, Option[Any]](
    category = NoSchema.Category.Option,
    element = element,
    nullable = false
  ) {
    override lazy val scalaType =
      new ReflectedScalaType(runtimeType).asInstanceOf[NoSchema.ScalaType[Option[Any]]]
  }
}
