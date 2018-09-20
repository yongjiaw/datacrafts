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

  override def toString: String = s"NoSchema[${scalaType.uniqueKey}](" +
    s"nullable=${nullable}, ${category})"
}

object NoSchema extends Slf4jLogging.Default {

  // this is for future extensibility to require extra information about the type
  implicit def noSchemaType[T: TypeTag : ClassTag : Manifest]: ScalaType[T] = this.synchronized {
    logDebug(s"creating ScalaType[classTag=${implicitly[ClassTag[T]]}]")
    lazy val uniqueKey = implicitly[TypeTag[T]].tpe.uniqueKey

    _scalaTypeInstances.getOrElseUpdate(
      uniqueKey,
      {
        logDebug(s"creating ScalaType[${uniqueKey}]")
        new ScalaType[T](uniqueKey)
      }
    )
  }.asInstanceOf[ScalaType[T]]

  case class TypeUniqueKey(fullName: String, typeArgs: Seq[TypeUniqueKey]) {
    override def toString: String = {
      s"${fullName}${if (typeArgs.isEmpty) "" else typeArgs.mkString("[", ",", "]")}"
    }
  }

  private val _scalaTypeInstances = collection.mutable.Map.empty[TypeUniqueKey, ScalaType[_]]

  implicit class TypeTagConverter(tpe: scala.reflect.runtime.universe.Type) {
    def uniqueKey: TypeUniqueKey = {
      logDebug(s"creating unique key for ${tpe.typeSymbol.fullName}")
      val uk = TypeUniqueKey(
        fullName = tpe.typeSymbol.fullName,
        typeArgs = tpe.dealias.typeArgs.map(_.uniqueKey)
      )
      logDebug(s"created uniqueKey ${uk}")
      uk
    }
  }

  class ScalaType[T: TypeTag : ClassTag : Manifest](val uniqueKey: TypeUniqueKey) {
    lazy val typeTag = implicitly[TypeTag[T]]
    lazy val classTag = implicitly[ClassTag[T]]
    lazy val manifest = implicitly[Manifest[T]]
    lazy val fullName: String = typeTag.tpe.typeSymbol.fullName
    lazy val shortName: String = typeTag.tpe.typeSymbol.name.toString
    override def toString: String = s"ScalaType[${uniqueKey}]"
    def matchInput(input: Any): Option[T] = classTag.unapply(input)
  }

  object Category extends Enumeration {
    val Primitive, Product, CoProduct, Option, Seq, Map, Set, Any = Value
  }

  trait HasLazySchema[T] {
    def lazySchema: NoSchema[T]

    override def toString: String = s"${lazySchema}"
  }

  private val _instances = collection.mutable.Map.empty[TypeUniqueKey, NoSchema[_]]

  def schemaInstances: Map[TypeUniqueKey, NoSchema[_]] = _instances.toMap

  def typeInstances: Map[TypeUniqueKey, ScalaType[_]] = _scalaTypeInstances.toMap

  // invoked when composing the final product and coproduct
  // root schema (product or coproduct) will invoke this too
  def getOrElseCreateSchema[T: ScalaType](creator: => NoSchema[T]): NoSchema[T] =
    this.synchronized {
      val scalaType = implicitly[ScalaType[T]]
      val reference = scalaType.uniqueKey
      val instance =
        _instances.getOrElseUpdate(
          reference,
          {
            logDebug(s"${reference} is new, creating instance. " +
              s"${_instances.size} registered instances")
            creator
          }
        )
      logDebug(s"${reference} found or added. ${_instances.size} registered instances")
      instance.asInstanceOf[NoSchema[T]]
    }

  private val _stackTraceMark = collection.mutable.Map.empty[TypeUniqueKey, Int]
  // Understanding this requires some knowledge of how the schema are created with shapeless and
  // recursive implicit resolution.
  // This method is invoked for all lazy instances (created by shapeless), as intermediate inputs
  // to the recursive implicit resolution.
  // To prevent infinite recursion with cyclic reference, each schema is marked on the call stack.
  // Although the call stack is local, there is no way to pass the call stack mark since many calls
  // are created by shapeless macro. The solution here is to use a global stack mark.
  // the root schema won't trigger this
  def getLazySchema[T: ScalaType](shapelessLazySchema: Lazy[NoSchema[T]]): HasLazySchema[T] =
    this.synchronized {

      def stackTraceDepth = Thread.currentThread().getStackTrace().size
      val scalaType = implicitly[ScalaType[T]]
      val reference = scalaType.uniqueKey
      if (_instances.contains(reference)) {
        logDebug(s"${reference} schema has already been created")
      }
      else {
        _stackTraceMark.get(reference) match {
          case Some(stackFrameDepth) =>
            logDebug(s"${reference} has already been marked on the schema create stack at " +
              s"depth=${stackFrameDepth}, current depth=${stackTraceDepth}, " +
              s"and will be created after returning. " +
              s"${_stackTraceMark.size} schema being created along the stack"
            )
          case None =>
            logDebug(s"${reference} has not been created, " +
              s"invoke lazy instance, current depth=${stackTraceDepth}. " +
              s"${_instances.size} registered instances")
            _stackTraceMark += reference -> stackTraceDepth
            // invoking shapelessLazySchema.value will trigger creating the schema which may
            // invoke creating the same schema if there's cyclic reference.
            // therefore, must mark it first to avoid infinite recursion
            // this is to leave marks along the recursive call stack
            try {
              _instances.put(reference, shapelessLazySchema.value)
              logDebug(s"${reference} schema created, ${_instances.size} registered instances")
            }
            finally {
              // remove the stackTrace mark after returning
              _stackTraceMark -= reference
              logDebug(s"${reference} stackTrace mark removed, " +
                s"${_stackTraceMark.size} still in stack.")
            }
        }

      }

      new HasLazySchema[T] {
        override def lazySchema: NoSchema[T] = {
          _instances.get(reference).getOrElse(
            throw new Exception(
              s"${reference} cannot be resolved from ${_instances.size} registered, " +
                s"this is not possible\n${
                _instances.keys.toSeq.map(_.toString).sorted.mkString("\n")
              }")
          )
        }.asInstanceOf[NoSchema[T]]
      }
    }
}
