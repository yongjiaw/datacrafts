package org.datacrafts.noschema.implicits

import org.datacrafts.noschema.{Context, NoSchema, Primitive}
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.Primitive.Type
import shapeless.Lazy

object Basics {

  trait Primitives {
    implicit val intPrimitiveType = new Primitive[Int](Type.Int)
    implicit val shortPrimitiveType = new Primitive[Short](Type.Short)
    implicit val longPrimitiveType = new Primitive[Long](Type.Long)
    implicit val floatPrimitiveType = new Primitive[Float](Type.Float)
    implicit val doublePrimitiveType = new Primitive[Double](Type.Double)
    implicit val booleanPrimitiveType = new Primitive[Boolean](Type.Boolean)
    implicit val stringPrimitiveType = new Primitive[String](Type.String, true)
    implicit val bytesPrimitiveType = new Primitive[Array[Byte]](Type.Bytes, true)
  }

  trait Containers {
    implicit def getOptionSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Option[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): OptionContainer[T] =
      new OptionContainer[T](Context.ContainerElement(node.value)
      )(ot.value, st.value)

    implicit def getSeqSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Seq[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): SeqContainer[T] =
      new SeqContainer[T](Context.ContainerElement(node.value)
      )(ot.value, st.value)

    implicit def getSetSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Set[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): SetContainer[T] =
      new SetContainer[T](Context.ContainerElement(node.value)
      )(ot.value, st.value)

    implicit def getIterableSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Iterable[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): IterableContainer[T] =
      new IterableContainer[T](Context.ContainerElement(node.value)
      )(ot.value, st.value)

    implicit def getMapSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Map[String, T]]],
      st: Lazy[NoSchema.ScalaType[T]]): MapContainer[T] =
      new MapContainer[T](Context.ContainerElement(node.value)
      )(ot.value, st.value)

    implicit def getMapSchemaFromElementSchema2[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[scala.collection.Map[String, T]]],
      st: Lazy[NoSchema.ScalaType[T]]): MapContainer2[T] =
      new MapContainer2[T](Context.ContainerElement(node.value)
      )(ot.value, st.value)
  }
}
