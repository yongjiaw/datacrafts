package org.datacrafts.noschema

class Primitive[T: NoSchema.ScalaType](
  val refinedType: Primitive.Type.Value,
  nullable: Boolean = false)
  extends NoSchema[T](
    category = NoSchema.Category.Primitive,
    nullable = nullable
  ) {
}

object Primitive {

  object Type extends Enumeration {
    val Int, Short, Long, Boolean, Bytes, String, Double, Float = Value
  }

  trait Instances {

    implicit val intPrimitiveType = new Primitive[Int](Type.Int)
    implicit val shortPrimitiveType = new Primitive[Short](Type.Short)
    implicit val longPrimitiveType = new Primitive[Long](Type.Long)
    implicit val floatPrimitiveType = new Primitive[Float](Type.Float)
    implicit val doublePrimitiveType = new Primitive[Double](Type.Double)
    implicit val booleanPrimitiveType = new Primitive[Boolean](Type.Boolean)
    implicit val stringPrimitiveType = new Primitive[String](Type.String, true)
    implicit val bytesPrimitiveType = new Primitive[Array[Byte]](Type.Bytes, true)
  }

}

