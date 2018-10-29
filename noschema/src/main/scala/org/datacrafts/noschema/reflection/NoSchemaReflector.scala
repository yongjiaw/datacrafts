package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{AnyType, Context, NoSchema}
import org.datacrafts.noschema.NoSchema.{HasLazySchema, ScalaType, TypeUniqueKey}

object NoSchemaReflector extends ReflectionDsl with Slf4jLogging.Default {

  import NoSchema.TypeTagConverter
  class ReflectedScalaType(runtimeType: ru.Type)
    extends ScalaType[Any](runtimeType.uniqueKey) {
    override lazy val tpe = runtimeType
    override lazy val classTag = throw new Exception(s"classTag not available for $this")
    override lazy val manifest = throw new Exception(s"manifest not available for $this")
    override def toString: String = s"RuntimeType[${uniqueKey}]"

    override def matchInput(input: Any): Option[Any] =
      if (input.getClass.getCanonicalName.stripSuffix("$") == tpe.typeSymbol.fullName) {
        Some(input)
      } else {
        Option.empty
      }
  }

  trait ReflectionRule {

    protected def coproductFilter(symbol: ru.Symbol): Boolean = {
      val result = symbol.name.toString != "UnknownUnionField"
      logDebug(s"filtering ${symbol}, result=$result")
      result
    }

    protected def productFilter(symbol: ru.Symbol): Boolean = true

    implicit def typeToTypeReflector(tpe: ru.Type): TypeReflector = {
      TypeReflector(tpe)
    }

    import scala.reflect.runtime.universe.typeOf
    def reflect(originalType: ru.Type): NoSchema[Any] = {
      val tpe = originalType.dealias
      tpe match {
        case t if t < typeOf[Int] => implicitly[NoSchema[Int]]
        case t if t < typeOf[Short] => implicitly[NoSchema[Short]]
        case t if t < typeOf[Long] => implicitly[NoSchema[Long]]
        case t if t < typeOf[Double] => implicitly[NoSchema[Double]]
        case t if t < typeOf[Boolean] => implicitly[NoSchema[Boolean]]
        case t if t < typeOf[Array[Byte]] => implicitly[NoSchema[Array[Byte]]]
        case t if t < typeOf[String] => implicitly[NoSchema[String]]
        case t if t =:= typeOf[Any] => AnyType

        // more specific type should go first
        // If Iterable schema was assigned to Set type, for example, unmarshalling/unapply is fine
        // it will cause type argument mismatch error when marshalling the Iterable value to Set
        case t if t < typeOf[Map[String, _]] =>
          val elementType = t.typeArgs(1)
          reflectMap(t, Context.ContainerElement(reflect(elementType)))

        case t if t < typeOf[Set[_]] =>
          val elementType = t.typeArgs(0)
          reflectSet(t, Context.ContainerElement(reflect(elementType)))

        case t if t < typeOf[Iterable[_]] =>
          val elementType = t.typeArgs(0)
          reflectSeq(t, Context.ContainerElement(reflect(elementType)))

        case t if t < typeOf[Option[_]] =>
          val elementType = t.typeArgs(0)
          reflectOption(t, Context.ContainerElement(reflect(elementType)))

        case _ =>

          logInfo(s"performing reflection on ${tpe.typeSymbol.fullName}")
          val reflector = TypeReflector(tpe)
          if (tpe.typeSymbol.isModuleClass) {
            logInfo(
              s"${tpe.typeSymbol.fullName} is Module(object)")
            new ReflectedProduct(tpe, Map.empty)
          }
          else if (reflector.caseAccessors.nonEmpty) {
            logInfo(
              s"${tpe.typeSymbol.fullName} is case class with type parameters=${tpe.typeArgs}")
            new ReflectedProduct(
              tpe,
              fields = (
                for (s <- reflector.applyArgs if productFilter(s)) yield {

                  val symbolName = s.name.toString
                  val symbolType =
                    reflector.caseMemberTypeMap.get(symbolName).getOrElse(
                      throw new Exception(
                        s"${reflector.fullName} arg symbol=${symbolName} " +
                          s"does not have case accessor")
                    )
                  s -> Context.MemberVariable(
                    Symbol(symbolName),

                    getOrCreateLazySchema(
                      symbolType,
                      reflect(symbolType.dealias)
                    )
                  )
              }).toMap
            )
          }
          else if (reflector.subclasses.nonEmpty) { // coproduct
            logInfo(
              s"${tpe.typeSymbol.fullName} is coproduct of (${reflector.subclasses})")
            new ReflectedCoproduct(
              tpe,
              members = (
                for (s <- reflector.subclasses if coproductFilter(s)) yield {
                  val symbolName = s.name.toString
                  ReflectedCoproduct.Member(
                    s,
                    None,
                    Context.CoproductElement(
                      Symbol(symbolName),
                      reflect(s.typeSignature.dealias)
                    )
                  )
              }).toSeq
            )
          }
          else {
            throw new Exception(s"${tpe.typeSymbol.fullName} not recognized")
          }

      }}.asInstanceOf[NoSchema[Any]]

    protected def getOrCreateLazySchema(
      tpe: ru.Type,
      creator: => NoSchema[Any]
    ): HasLazySchema[Any] = {

      import NoSchema.TypeTagConverter
      val reference = tpe.dealias.uniqueKey

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

  }

  private val _stackTraceMark = collection.mutable.Map.empty[TypeUniqueKey, Int]

  private val _instances = collection.mutable.Map.empty[TypeUniqueKey, NoSchema[Any]]

  def schemaInstances: Map[TypeUniqueKey, NoSchema[Any]] = _instances.toMap

}
