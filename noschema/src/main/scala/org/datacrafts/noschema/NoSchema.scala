package org.datacrafts.noschema

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.Context.LocalContext

/**
  * Base NoSchema class
  */
abstract class NoSchema[T: NoSchema.Type](
  val category: NoSchema.Category.Value,
  val nullable: Boolean,
  val dependencies: Seq[LocalContext[_]] = Seq.empty
) extends Slf4jLogging.Default {

  logDebug(s"constructing ${this}")

  final lazy val tpe = implicitly[NoSchema.Type[T]].tpe

  override def toString: String = s"${
    if (category == NoSchema.Category.Primitive ||
      category == NoSchema.Category.Struct) {
      tpe
    } else {
      category
    }
  }(nullable = ${nullable})"

}

object NoSchema  {

  // this is for future extensibility to require extra information about the type
  implicit def noSchemaType[T: TypeTag : ClassTag : Manifest]: Type[T] = new Type[T]

  class Type[T: TypeTag : ClassTag : Manifest] {
    lazy val tpe = implicitly[TypeTag[T]].tpe

    override def toString: String = s"NoSchema.Type[${tpe}]"
  }

  object Category extends Enumeration {
    val Seq, Map, Struct, Primitive, Option = Value
  }

}

