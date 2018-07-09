package org.datacrafts.noschema

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.Context.LocalContext
import shapeless.Lazy

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
    s"nullable=${nullable}, ${category})"
}

object NoSchema extends Slf4jLogging.Default {

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

    val hasTypeArgs: Boolean = {
      typeTag.tpe.typeArgs.nonEmpty || typeTag.tpe.dealias.typeArgs.nonEmpty
    }
  }

  object Category extends Enumeration {
    val Primitive, Product, CoProduct, Option, Seq, Map = Value
  }

  trait HasLazySchema[T] {
    def lazySchema: NoSchema[T]
  }

  private val _instances = collection.mutable.Map.empty[String, NoSchema[_]]
  private val _instanceMarks = collection.mutable.Set.empty[String]

  def getLazySchema[T: ScalaType](shapelessLazySchema: Lazy[NoSchema[T]]): HasLazySchema[T] =
    _instanceMarks.synchronized {
    val scalaType = implicitly[ScalaType[T]]
    if (scalaType.hasTypeArgs) {
      // nested type parameters with cyclic structure is not supported by shapeless
      // therefore, can ignore any type with type arguments
      // composition nesting with cyclic structure can be supported by using
      // lazy reference
      logDebug(s"${scalaType.fullName} has type args, create new instance")
      new HasLazySchema[T] {
        override def lazySchema: NoSchema[T] = shapelessLazySchema.value
      }
    } else {
      logDebug(s"${scalaType.fullName} is concrete type, " +
        s"resolve lazily to deal with cyclic reference")
      val reference = scalaType.fullName
      if (!_instanceMarks.contains(reference)) {
        logDebug(s"${scalaType.fullName} is new, mark and create instance")
        _instanceMarks += reference
        _instances.put(
          reference, shapelessLazySchema.value
        )
      } else {
        logDebug(s"${scalaType.fullName} has already been marked")
      }
      new HasLazySchema[T] {
        override def lazySchema: NoSchema[T] = {
          _instances.get(reference).getOrElse(
            throw new Exception(
              s"${reference} cannot be resolved, make sure to invoke through lazy or def")
          )
        }.asInstanceOf[NoSchema[T]]
      }
    }


  }
}
