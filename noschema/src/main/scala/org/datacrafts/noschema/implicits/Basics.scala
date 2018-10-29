package org.datacrafts.noschema.implicits

import org.datacrafts.noschema.{Container, Context, NoSchema, Primitive}
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
    ): Container[T, Option[T]] =
      new Container[T, Option[T]](
        NoSchema.Category.Option,
        Context.ContainerElement(node.value), false)(st.value, ot.value)

    implicit def getSeqSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Seq[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): Container[T, Seq[T]] =
      new Container[T, Seq[T]](
        NoSchema.Category.Seq,
        Context.ContainerElement(node.value))(st.value, ot.value)

    implicit def getSetSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Set[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): Container[T, Set[T]] =
      new Container[T, Set[T]](
        NoSchema.Category.Set,
        Context.ContainerElement(node.value))(st.value, ot.value)

    implicit def getIterableSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Iterable[T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): Container[T, Iterable[T]] =
      new Container[T, Iterable[T]](
        NoSchema.Category.Seq,
        Context.ContainerElement(node.value))(st.value, ot.value)

    implicit def getMapSchemaFromElementSchema[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[Map[String, T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): Container[T, Map[String, T]] =
      new Container[T, Map[String, T]](
        NoSchema.Category.Map,
        Context.ContainerElement(node.value))(st.value, ot.value)

    implicit def getMapSchemaFromElementSchema2[T](implicit
      node: Lazy[NoSchema[T]],
      ot: Lazy[NoSchema.ScalaType[scala.collection.Map[String, T]]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): Container[T, scala.collection.Map[String, T]] =
      new Container[T, scala.collection.Map[String, T]](
        NoSchema.Category.Map,
        Context.ContainerElement(node.value))(st.value, ot.value)
  }
}
