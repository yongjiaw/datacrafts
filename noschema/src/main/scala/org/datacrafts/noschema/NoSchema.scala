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

  override def toString: String = s"NoSchema[${scalaType.fullName}](" +
    s"nullable=${nullable}, ${category}${dependencies.map(_.noSchema).mkString("{", ",", "}")})"

}

object NoSchema {

  // this is for future extensibility to require extra information about the type
  implicit def noSchemaType[T: TypeTag : ClassTag : Manifest]: ScalaType[T] = new ScalaType[T]

  class ScalaType[T: TypeTag : ClassTag : Manifest] {
    // typeTag.tpe.toString can get error:
    // unsafe symbol Generated (child of package annotation) in runtime reflection universe
    lazy val typeTag = implicitly[TypeTag[T]]
    lazy val classTag = implicitly[ClassTag[T]]
    lazy val manifest = implicitly[Manifest[T]]

    def fullName: String = typeTag.tpe.typeSymbol.fullName

    override def toString: String = s"ScalaType[${fullName}]"
  }

  object Category extends Enumeration {
    val Primitive, Product, CoProduct, Option, Seq, Map = Value
  }

}
