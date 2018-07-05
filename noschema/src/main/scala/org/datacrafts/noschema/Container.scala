package org.datacrafts.noschema

abstract class Container[T: NoSchema.Type, C: NoSchema.Type](
  category: NoSchema.Category.Value,
  element: Context.ContainerElement[T],
  nullable: Boolean = true
) extends NoSchema[C](
    category = category, nullable = nullable, dependencies = Seq(element)
  ) {
  type Elem = T
}

object Container {

  case class OptionContainer[T: NoSchema.Type](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.Type[Option[T]])
    extends Container[T, Option[T]](NoSchema.Category.Option, element, false)

  case class SeqContainer[T: NoSchema.Type](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.Type[Seq[T]])
    extends Container[T, Seq[T]](NoSchema.Category.Seq, element)

  case class IterableContainer[T: NoSchema.Type](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.Type[Iterable[T]])
    extends Container[T, Iterable[T]](NoSchema.Category.Seq, element)

  case class MapContainer[T: NoSchema.Type](element: Context.ContainerElement[T])
    (implicit ot: NoSchema.Type[Map[String, T]])
    extends Container[T, Map[String, T]](NoSchema.Category.Seq, element)

  trait Instances {
    implicit def getOptionSchemaFromElementSchema[T: NoSchema.Type](implicit
      node: NoSchema[T],
      ot: NoSchema.Type[Option[T]]): OptionContainer[T] =
      new OptionContainer[T](Context.ContainerElement(node))

    implicit def getSeqSchemaFromElementSchema[T: NoSchema.Type](implicit
      node: NoSchema[T],
      ot: NoSchema.Type[Seq[T]]): SeqContainer[T] =
      new SeqContainer[T](Context.ContainerElement(node))

    implicit def getIterableSchemaFromElementSchema[T: NoSchema.Type](implicit
      node: NoSchema[T],
      ot: NoSchema.Type[Iterable[T]]): IterableContainer[T] =
      new IterableContainer[T](Context.ContainerElement(node))

    implicit def getMapSchemaFromElementSchema[T: NoSchema.Type](implicit
      node: NoSchema[T],
      ot: NoSchema.Type[Map[String, T]]): MapContainer[T] =
      new MapContainer[T](Context.ContainerElement(node))
  }

}
