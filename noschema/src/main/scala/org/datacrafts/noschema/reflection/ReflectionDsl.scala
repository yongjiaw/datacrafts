package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.noschema.{Container, Context, NoSchema}
import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectedScalaType

trait ReflectionDsl {

  def reflectContainer[C: NoSchema.ScalaType](
    runtimeType: ru.Type,
    element: Context.ContainerElement[Any],
    category: NoSchema.Category.Value,
    nullable: Boolean
  ): Container[Any, C] = {
    new Container[Any, C](
      category = category,
      element = element,
      nullable = nullable
    ) {
      override lazy val scalaType =
        new ReflectedScalaType(runtimeType).asInstanceOf[NoSchema.ScalaType[C]]
    }
  }

  def reflectOption(
    runtimeType: ru.Type,
    element: Context.ContainerElement[Any]
  ): Container[Any, Option[Any]] =
    reflectContainer[Option[Any]](
      runtimeType,
      element,
      NoSchema.Category.Option,
      nullable = false
    )

  def reflectSeq(
    runtimeType: ru.Type,
    element: Context.ContainerElement[Any]
  ): Container[Any, Iterable[Any]] =
    reflectContainer[Iterable[Any]](
      runtimeType,
      element,
      NoSchema.Category.Seq,
      nullable = true
    )

  def reflectSet(
    runtimeType: ru.Type,
    element: Context.ContainerElement[Any]
  ): Container[Any, Iterable[Any]] =
    reflectContainer[Iterable[Any]](
      runtimeType,
      element,
      NoSchema.Category.Set,
      nullable = true
    )

  def reflectMap(
    runtimeType: ru.Type,
    element: Context.ContainerElement[Any]
  ): Container[Any, Iterable[(String, Any)]] =
    reflectContainer[Iterable[(String, Any)]](
      runtimeType,
      element,
      NoSchema.Category.Map,
      nullable = true
    )

}
