package org.datacrafts.noschema

import shapeless.Lazy

abstract class Container[T: NoSchema.ScalaType, C: NoSchema.ScalaType](
  category: NoSchema.Category.Value,
  val element: Context.ContainerElement[T],
  nullable: Boolean = true
) extends NoSchema[C](
    category = category, nullable = nullable, dependencies = Seq(element)
  ) {
  type Elem = T
}

object Container {

  case class OptionContainer[T](override val element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Option[T]],
      st: NoSchema.ScalaType[T])
    extends Container[T, Option[T]](NoSchema.Category.Option, element, false)

  case class SeqContainer[T](override val element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Seq[T]],
      st: NoSchema.ScalaType[T])
    extends Container[T, Seq[T]](NoSchema.Category.Seq, element)

  case class IterableContainer[T](override val element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Iterable[T]],
      st: NoSchema.ScalaType[T])
    extends Container[T, Iterable[T]](NoSchema.Category.Seq, element)

  case class MapContainer[T](override val element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Map[String, T]],
      st: NoSchema.ScalaType[T])
    extends Container[T, Map[String, T]](NoSchema.Category.Map, element)

  // to support multiple scala Map concrete types
  case class MapContainer2[T](override val element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[scala.collection.Map[String, T]],
      st: NoSchema.ScalaType[T])
    extends Container[T, scala.collection.Map[String, T]](NoSchema.Category.Map, element)

  trait Instances {
    implicit def getOptionSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Option[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): OptionContainer[T] =
      new OptionContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)(st.value))
      )(ot.value, st.value)

    implicit def getSeqSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Seq[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): SeqContainer[T] =
      new SeqContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)(st.value))
      )(ot.value, st.value)

    implicit def getIterableSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Iterable[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): IterableContainer[T] =
      new IterableContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)(st.value))
      )(ot.value, st.value)

    implicit def getMapSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Map[String, T]]],
      st: Lazy[NoSchema.ScalaType[T]]): MapContainer[T] =
      new MapContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)(st.value))
      )(ot.value, st.value)

    implicit def getMapSchemaFromElementSchema2[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[scala.collection.Map[String, T]]],
      st: Lazy[NoSchema.ScalaType[T]]): MapContainer2[T] =
      new MapContainer2[T](Context.ContainerElement(NoSchema.getLazySchema(node)(st.value))
      )(ot.value, st.value)

  }

}
