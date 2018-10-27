package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.noschema.{Container, Context, NoSchema}

object ReflectedContainer {

  class RfOption(
    runtimeType: ru.Type,
    element: Context.ContainerElement[Any]
  ) extends Container[Any, Option[Any]](
    category = NoSchema.Category.Option,
    element = element,
    nullable = false
  ) {
    import org.datacrafts.noschema.NoSchema._
    override lazy val scalaType: NoSchema.ScalaType[Option[Any]] =
      new NoSchema.ScalaType[Option[Any]](runtimeType.uniqueKey) {
        override lazy val tpe = runtimeType

        override def toString: String = s"RuntimeType[${uniqueKey}]"

        override def matchInput(input: Any): Option[Option[Any]] = Option.empty
      }
  }
}
