package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{AnyType, Context, NoSchema}
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.NoSchema.{_instances, _stackTraceMark, logDebug, logInfo, HasLazySchema, ScalaType, TypeUniqueKey}
import org.datacrafts.noschema.reflection.NoSchemaReflector.logInfo

object NoSchemaReflector extends Slf4jLogging.Default {

  import scala.reflect.runtime.universe.typeOf

  trait ReflectionRule {

    implicit def noschemaWrapper[T](noSchema: NoSchema[T]): HasLazySchema[T] =
      new HasLazySchema[T] {
        override def lazySchema: NoSchema[T] = noSchema
      }

    implicit class NoSchemaLazyConverter[T](noSchema: NoSchema[T]) {
      def asLazy: HasLazySchema[Any] = new HasLazySchema[Any] {
        override def lazySchema: NoSchema[Any] = noSchema.asInstanceOf[NoSchema[Any]]
      }
    }

    def reflect(tpe: ru.Type): NoSchema[Any] = (
      tpe match {
        case t if t <:< typeOf[Int] => implicitly[NoSchema[Int]]
        case t if t <:< typeOf[Short] => implicitly[NoSchema[Short]]
        case t if t <:< typeOf[Long] => implicitly[NoSchema[Long]]
        case t if t <:< typeOf[Double] => implicitly[NoSchema[Double]]
        case t if t <:< typeOf[Boolean] => implicitly[NoSchema[Boolean]]
        case t if t <:< typeOf[Array[Byte]] => implicitly[NoSchema[Array[Byte]]]
        case t if t <:< typeOf[String] => implicitly[NoSchema[String]]
        case t if t =:= typeOf[Any] => AnyType

          // more specific type should go first
          // If Iterable schema was assigned to Set type, for example, unmarshalling/unapply is fine
          // it will cause type argument mismatch error when marshalling the Iterable value to Set
        case t if t <:< typeOf[Map[String, _]] =>
          val elementType = t.typeArgs(0)
          MapContainer(Context.ContainerElement(reflect(elementType)))

        case t if t <:< typeOf[Set[_]] =>
          val elementType = t.typeArgs(0)
          SetContainer(Context.ContainerElement(reflect(elementType)))

        case t if t <:< typeOf[Seq[_]] =>
          val elementType = t.typeArgs(0)
          SeqContainer(Context.ContainerElement(reflect(elementType)))

        case t if t <:< typeOf[Iterable[_]] =>
          val elementType = t.typeArgs(0)
          IterableContainer(Context.ContainerElement(reflect(elementType)))

        case t if t <:< typeOf[Option[_]] =>
          val elementType = t.typeArgs(0)
          OptionContainer(Context.ContainerElement(reflect(elementType)))

        case _ =>

          logInfo(s"performing reflection on ${tpe.typeSymbol.fullName}")
          val reflector = new TypeReflector(tpe)
          if (reflector.caseAccessors.nonEmpty) {
            logInfo(
              s"${tpe.typeSymbol.fullName} is case class with type parameters=${tpe.typeArgs}")
            new ReflectedProduct(
              tpe,
              fields = reflector.applyArgs.map {
                s =>
                  val symbolName = s.name.toString
                  val symbolType = reflector.caseMemberTypeMap.get(symbolName).getOrElse(
                    throw new Exception(
                      s"${reflector.fullName} arg symbol=${symbolName} " +
                        s"does not have case accessor")
                  )
                  s -> Context.MemberVariable(
                    Symbol(symbolName),

                    getOrCreateLazySchema(
                      symbolType,
                      reflect(symbolType)
                    )
                  )
              }.toMap
            )
          }
          else if (reflector.subclasses.nonEmpty) { // coproduct
            logInfo(
              s"${tpe.typeSymbol.fullName} is coproduct of (${reflector.subclasses})")
            new ReflectedCoproduct(
              tpe,
              members = reflector.subclasses.map {
                s =>
                  val symbolName = s.name.toString
                  s -> Context.CoproductElement(
                    Symbol(symbolName),
                    reflect(s.typeSignature.dealias)
                  )
              }.toMap
            )
          }
          else {
            throw new Exception(s"${tpe.typeSymbol.fullName} not recognized")
          }

      }).asInstanceOf[NoSchema[Any]]

  }
  
  private def getOrCreateLazySchema(
    tpe: ru.Type,
    creator: => NoSchema[Any]
  ): HasLazySchema[Any] = {

    import NoSchema.TypeTagConverter
    val reference = tpe.uniqueKey

    this.synchronized {

      def stackTraceDepth: Int = Thread.currentThread().getStackTrace().size

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
              _instances.put(reference, creator)
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

      new HasLazySchema[Any] {
        override def lazySchema: NoSchema[Any] = {
          _instances.get(reference).getOrElse(
            throw new Exception(
              s"${reference} cannot be resolved from ${_instances.size} registered, " +
                s"this is not possible\n${
                  _instances.keys.toSeq.map(_.toString).sorted.mkString("\n")
                }")
          )
        }
      }
    }
  }
  private val _stackTraceMark = collection.mutable.Map.empty[TypeUniqueKey, Int]

  private val _instances = collection.mutable.Map.empty[TypeUniqueKey, NoSchema[Any]]

  def schemaInstances: Map[TypeUniqueKey, NoSchema[Any]] = _instances.toMap

}
