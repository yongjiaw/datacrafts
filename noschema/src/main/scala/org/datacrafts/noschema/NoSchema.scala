package org.datacrafts.noschema

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.Context.LocalContext
import org.datacrafts.noschema.rule.DefaultRule

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

  override def toString: String = s"${scalaType.tpe}(nullable = ${nullable})"

}

object NoSchema {

  // this is for future extensibility to require extra information about the type
  implicit def noSchemaType[T: TypeTag : ClassTag]: ScalaType[T] = new ScalaType[T]

  class ScalaType[T: TypeTag : ClassTag] {
    lazy val tpe = implicitly[TypeTag[T]].tpe

    override def toString: String = s"NoSchema.Type[${tpe}]"
  }

  object Category extends Enumeration {
    val Primitive, Product, CoProduct, Option, Seq, Map = Value
  }

  def of[T: NoSchema](implicit rule: Operation.Rule = DefaultRule): Operation[T] = {
    new Operation[T](Context.root(implicitly[NoSchema[T]]), rule)
  }
}
