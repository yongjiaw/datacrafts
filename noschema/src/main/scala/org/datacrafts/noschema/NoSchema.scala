package org.datacrafts.noschema

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.Context.LocalContext

/**
  * Base NoSchema class
  */
abstract class NoSchema[T: NoSchema.ScalaType](
  val category: NoSchema.Category.Value,
  val nullable: Boolean,
  val dependencies: Seq[LocalContext[_]] = Seq.empty
) extends Slf4jLogging.Default {

  logDebug(s"constructing ${this}")

  final lazy val scalaType = implicitly[NoSchema.ScalaType[T]]

  override def toString: String = s"${scalaType.typeString}(nullable = ${nullable})"

}

object NoSchema {

  // this is for future extensibility to require extra information about the type
  implicit def noSchemaType[T: TypeTag : ClassTag]: ScalaType[T] = new ScalaType[T]

  class ScalaType[T: TypeTag : ClassTag] {
    // this can get error:
    // unsafe symbol Generated (child of package annotation) in runtime reflection universe
    lazy val tpe = implicitly[TypeTag[T]].tpe

    lazy val classTag = implicitly[ClassTag[T]]
    lazy val typeString = classTag.toString()

    override def toString: String = s"NoSchema.Type[${classTag}]"
  }

  object Category extends Enumeration {
    val Primitive, Product, CoProduct, Option, Seq, Map = Value
  }

}
