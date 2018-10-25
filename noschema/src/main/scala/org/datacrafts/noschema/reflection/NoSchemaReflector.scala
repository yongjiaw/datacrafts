package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{AnyType, Context, NoSchema}
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.NoSchema.HasLazySchema

object NoSchemaReflector extends Slf4jLogging.Default {

  import scala.reflect.runtime.universe.typeOf

  trait ReflectionRule {

    implicit def noschemaWrapper[T](noSchema: NoSchema[T]): HasLazySchema[T] =
      new HasLazySchema[T] {
        override def lazySchema: NoSchema[T] = noSchema
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
          val reflector = new TypeReflector(tpe)
          if (reflector.caseAccessors.nonEmpty) {
            logInfo(
              s"${tpe.typeSymbol.fullName} is case class with type parameters=${tpe.typeArgs}")
            new ReflectedProduct(
              tpe,
              fields = reflector.applyArgs.map {
                s =>
                  val symbolName = s.name.toString
                  s -> Context.MemberVariable(
                    Symbol(symbolName),
                    reflect(
                      reflector.caseMemberTypeMap.get(symbolName).getOrElse(
                        throw new Exception(
                          s"${reflector.fullName} arg symbol=${symbolName} " +
                            s"does not have case accessor")
                      )
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
      }
      ).asInstanceOf[NoSchema[Any]]
  }

}
