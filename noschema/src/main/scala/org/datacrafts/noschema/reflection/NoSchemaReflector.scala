package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.noschema.{Context, NoSchema, NoSchemaDsl, Primitive}
import org.datacrafts.noschema.Container.SeqContainer
import org.datacrafts.noschema.NoSchema.HasLazySchema
import org.datacrafts.noschema.Primitive.Type

object NoSchemaReflector {

  import scala.reflect.runtime.universe.typeOf

  trait ReflectionRule {

    implicit def noschemaWrapper[T](noSchema: NoSchema[T]): HasLazySchema[T] =
      new HasLazySchema[T] {
        override def lazySchema: NoSchema[T] = noSchema
      }

    def reflect(tpe: ru.Type): NoSchema[Any] = (
      tpe match {
        case t if t <:< typeOf[Int] => new Primitive[Int](Type.Int)
        case t if t <:< typeOf[Seq[_]] =>
          val elementType = t.typeArgs(0)
          SeqContainer(Context.ContainerElement(reflect(elementType)))

        // val reflector = new Reflector(tpe)
        case _ => throw new Exception(s"${tpe.typeSymbol.fullName} not recognized")

      }
      ).asInstanceOf[NoSchema[Any]]
  }

}

object Test123 extends NoSchemaDsl {

  def main(args: Array[String]): Unit = {
    val noSchema = reflectedSchemaOf[Seq[Int]]()
    println(noSchema)
    println(noSchema.format())
    println(noSchema.operation().unmarshal(Seq(1)))
    println(noSchema.operation().marshal(Seq(1)))
  }
}
