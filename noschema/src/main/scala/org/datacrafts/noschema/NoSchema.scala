package org.datacrafts.noschema

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.{Failure, Success, Try}

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
  implicit def noSchemaType[T: TypeTag : ClassTag : Manifest]: ScalaType[T] = this.synchronized {
    val tpe = implicitly[TypeTag[T]].tpe
    val uniqueKey =
      if (tpe.typeArgs.nonEmpty || tpe.dealias.typeArgs.nonEmpty) {
        tpe.toString
      } else {
        // avoid using tpe.toString since it may encounter error
        // unsafe symbol Generated (child of package annotation) in runtime reflection universe
        // for org.datacrafts.scrooge.shapes.UnionExample
        // hope this is just a special case
        // for types with type arguments, we have to use tpe.toString
        tpe.typeSymbol.fullName
      }

    _scalaTypeInstances.getOrElseUpdate(
      uniqueKey,
      {
        logDebug(s"creating ScalaType[${uniqueKey}]")
        new ScalaType[T](uniqueKey)
      }
    )
  }.asInstanceOf[ScalaType[T]]

  private val _scalaTypeInstances = collection.mutable.Map.empty[String, ScalaType[_]]

  class ScalaType[T: TypeTag : ClassTag : Manifest](val uniqueKey: String) {
    lazy val typeTag = implicitly[TypeTag[T]]
    lazy val classTag = implicitly[ClassTag[T]]
    lazy val manifest = implicitly[Manifest[T]]
    lazy val fullName: String = typeTag.tpe.typeSymbol.fullName
    override def toString: String = s"ScalaType[${uniqueKey}]"
  }

  object Category extends Enumeration {
    val Primitive, Product, CoProduct, Option, Seq, Map = Value
  }

  trait HasLazySchema[T] {
    def lazySchema: NoSchema[T]
  }

  private val _instances = collection.mutable.Map.empty[String, NoSchema[_]]
  private val _instanceMarks = collection.mutable.Set.empty[String]

  def schemaInstances: Map[String, NoSchema[_]] = _instances.toMap
  def typeInstances: Map[String, ScalaType[_]] = _scalaTypeInstances.toMap

  // invoked when composing the final product and coproduct
  // root schema will invoked this too
  def getOrElseCreateSchema[T: ScalaType](creator: => NoSchema[T]): NoSchema[T] =
    this.synchronized {
      val scalaType = implicitly[ScalaType[T]]
      val reference = scalaType.uniqueKey
      val instance =
      _instances.getOrElseUpdate(
        reference,
        {
          logDebug(s"${reference} is new, create instance. ${_instances.size} instances")
          creator
        }
      )
      logDebug(s"${reference} found or added. ${_instances.size} instances")
      instance.asInstanceOf[NoSchema[T]]
  }

  // invoked for all lazy instances as intermediate inputs to the recursive implicit resolution
  // the root schema won't trigger this
  def getLazySchema[T: ScalaType](shapelessLazySchema: Lazy[NoSchema[T]]): HasLazySchema[T] =
    this.synchronized {
    val scalaType = implicitly[ScalaType[T]]
    val reference = scalaType.uniqueKey
    if (!_instanceMarks.contains(reference)) {
      logDebug(s"${reference} is new, mark then create instance. ${_instances.size} instances")
      _instanceMarks += reference
      // invoking shapelessLazySchema.value will trigger creating the schema which may
      // invoke creating the same schema if there's cyclic reference.
      // therefore, must mark it first to avoid infinite recursion
      // this is to leave marks along the recursive call stack
      Try(
      _instances.put(
        reference, shapelessLazySchema.value
      )) match {
        case Success(_) =>
          logDebug(s"${reference} added. ${_instances.size} instances")
        case Failure(f) =>
          _instanceMarks -= reference
          throw new Exception(s"failed to add instance ${reference}", f)
      }
    } else {
      logDebug(s"${reference} has already been marked, and will be created")
    }
    new HasLazySchema[T] {
      override def lazySchema: NoSchema[T] = {
        _instances.get(reference).getOrElse(
          throw new Exception(
            s"${reference} cannot be resolved, this is not possible")
        )
      }.asInstanceOf[NoSchema[T]]
    }
  }
}
