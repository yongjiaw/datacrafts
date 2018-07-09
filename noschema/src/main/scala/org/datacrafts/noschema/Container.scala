package org.datacrafts.noschema

import shapeless.Lazy

abstract class Container[T: NoSchema.ScalaType, C: NoSchema.ScalaType](
  category: NoSchema.Category.Value,
  element: Context.ContainerElement[T],
  nullable: Boolean = true
) extends NoSchema[C](
    category = category, nullable = nullable, dependencies = Seq(element)
  ) {
  type Elem = T
}

object Container {

  case class OptionContainer[T: NoSchema.ScalaType](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Option[T]])
    extends Container[T, Option[T]](NoSchema.Category.Option, element, false)

  case class SeqContainer[T: NoSchema.ScalaType](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Seq[T]])
    extends Container[T, Seq[T]](NoSchema.Category.Seq, element)

  case class IterableContainer[T: NoSchema.ScalaType](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Iterable[T]])
    extends Container[T, Iterable[T]](NoSchema.Category.Seq, element)

  case class MapContainer[T: NoSchema.ScalaType](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[Map[String, T]])
    extends Container[T, Map[String, T]](NoSchema.Category.Map, element)

  // to support multiple scala Map concrete types
  case class MapContainer2[T: NoSchema.ScalaType](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.ScalaType[scala.collection.Map[String, T]])
    extends Container[T, scala.collection.Map[String, T]](NoSchema.Category.Map, element)

  trait Instances {
    implicit def getOptionSchemaFromElementSchema[T: NoSchema.ScalaType](implicit
      node: Lazy[NoSchema[T]],
      ot: NoSchema.ScalaType[Option[T]]): OptionContainer[T] =
      new OptionContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)))

    implicit def getSeqSchemaFromElementSchema[T: NoSchema.ScalaType](implicit
      node: Lazy[NoSchema[T]],
      ot: NoSchema.ScalaType[Seq[T]]): SeqContainer[T] =
      new SeqContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)))

    implicit def getIterableSchemaFromElementSchema[T: NoSchema.ScalaType](implicit
      node: Lazy[NoSchema[T]],
      ot: NoSchema.ScalaType[Iterable[T]]): IterableContainer[T] =
      new IterableContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)))

    implicit def getMapSchemaFromElementSchema[T: NoSchema.ScalaType](implicit
      node: Lazy[NoSchema[T]],
      ot: NoSchema.ScalaType[Map[String, T]]): MapContainer[T] =
      new MapContainer[T](Context.ContainerElement(NoSchema.getLazySchema(node)))

    implicit def getMapSchemaFromElementSchema2[T: NoSchema.ScalaType](implicit
      node: Lazy[NoSchema[T]],
      ot: NoSchema.ScalaType[scala.collection.Map[String, T]]): MapContainer2[T] =
      new MapContainer2[T](Context.ContainerElement(NoSchema.getLazySchema(node)))

  }

}
