package org.datacrafts.noschema

import org.datacrafts.noschema.ShapelessCoproduct.{ShapelessCoproductAdapter, TypeValueExtractor, UnionTypeValueCollector}
import org.datacrafts.noschema.ShapelessProduct.FallbackImplicits
import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}

class ShapelessCoproduct[T : NoSchema.ScalaType, R <: Coproduct](
  dependencies: Seq[Context.CoproductElement[_]],
  generic: LabelledGeneric.Aux[T, R],
  shapeless: ShapelessCoproductAdapter[R]
) extends NoSchema[T](
  category = NoSchema.Category.CoProduct,
  nullable = true,
  dependencies = dependencies
)
{
  def marshal(typeExtractor: TypeValueExtractor, operation: Operation[T]): T = {
    generic.from(shapeless.marshalCoproduct(typeExtractor, operation))
  }

  def unmarshal(input: T, emptyUnion: UnionTypeValueCollector, operation: Operation[T]
  ): UnionTypeValueCollector = {
    shapeless.unmarshalCoproduct(generic.to(input), emptyUnion, operation)
  }
}

object ShapelessCoproduct {

  trait FallbackImplicits {

    // this helps pass compile time check, and will throw runtime error
    // if certain type transformer is missing
    implicit def lowPriorityFallBackImplicit[V: NoSchema.ScalaType, L <: Coproduct](implicit
      l: ShapelessCoproductAdapter[L]
    ): ShapelessCoproductAdapter[V :+: L] = {
      throw new Exception(s"coproduct member ${implicitly[NoSchema.ScalaType[V]]} " +
        s"does not have NoSchema in scope.")
    }
  }

  trait Instances extends FallbackImplicits {
    implicit def shapelessCoproductRecursiveBuilder[K <: Symbol, V, L <: Coproduct](implicit
      headSymbol: Witness.Aux[K],
      head: Lazy[NoSchema[V]],
      tail: Lazy[ShapelessCoproductAdapter[L]]
    ): ShapelessCoproductAdapter[FieldType[K, V] :+: L] = {

      val headValueContext =
        Context.CoproductElement(headSymbol.value, head.value)

      new ShapelessCoproductAdapter[FieldType[K, V] :+: L](
        members = tail.value.members :+ headValueContext) {

        override def marshalCoproduct(
          typeValueExtractor: TypeValueExtractor, operation: Operation[_]
        ): FieldType[K, V] :+: L = {
          typeValueExtractor.getTypeValue(head.value.scalaType) match {
            case Some(value) =>
              Inl[FieldType[K, V], L](
                field[K](operation.dependencyOperation(headValueContext).operator.marshal(value)))
            case None =>
              Inr[FieldType[K, V], L](tail.value.marshalCoproduct(typeValueExtractor, operation))
          }
        }

        override def unmarshalCoproduct(
          coproduct: FieldType[K, V] :+: L,
          emptyUnion: UnionTypeValueCollector, operation: Operation[_]
        ): UnionTypeValueCollector = {
          coproduct match {
            case Inl(headValue) => emptyUnion.addTypeValue(head.value.scalaType, headValue)
            case Inr(tailValue) => tail.value.unmarshalCoproduct(
              tailValue, emptyUnion, operation
            )
          }
        }
      }
    }

    implicit def shapelessCoproductRecursiveBuilderTerminator[K <: Symbol, V](implicit
      headSymbol: Witness.Aux[K],
      headValue: NoSchema[V]
    ): ShapelessCoproductAdapter[FieldType[K, V] :+: CNil] = {

      val headValueContext =
        Context.CoproductElement(headSymbol.value, headValue)

      new ShapelessCoproductAdapter[FieldType[K, V] :+: CNil](
        members = Seq(headValueContext)) {

        override def marshalCoproduct(
          typeExtractor: TypeValueExtractor, operation: Operation[_]): FieldType[K, V] :+: CNil = {
          typeExtractor.getTypeValue(headValue.scalaType) match {
            case Some(value) =>
              Inl[FieldType[K, V], CNil](
                field[K](operation.dependencyOperation(headValueContext).operator.marshal(value)))
            case None =>
              throw new Exception(s"no value found for any type")
          }
        }

        override def unmarshalCoproduct(
          coproduct: FieldType[K, V] :+: CNil,
          emptyUnion: UnionTypeValueCollector, operation: Operation[_]
        ): UnionTypeValueCollector = {
          coproduct match {
            case Inl(value) => emptyUnion.addTypeValue(headValue.scalaType, value)
            case _ => throw new Exception("impossible")
          }
        }
      }
    }

    implicit def shapelessCoproductBridging[T: NoSchema.ScalaType, R <: Coproduct](implicit
      generic: LabelledGeneric.Aux[T, R],
      shapeless: Lazy[ShapelessCoproductAdapter[R]]
    ): NoSchema[T] = shapeless.value.composeWithGeneric(generic)
  }

  abstract class ShapelessCoproductAdapter[R <: Coproduct](
    val members: Seq[Context.CoproductElement[_]]
  ) {
    def marshalCoproduct(typeExtractor: TypeValueExtractor, operation: Operation[_]): R

    def unmarshalCoproduct(
      coproduct: R, emptyUnion: UnionTypeValueCollector, operation: Operation[_]
    ): UnionTypeValueCollector

    def composeWithGeneric[T: NoSchema.ScalaType](
      generic: LabelledGeneric.Aux[T, R]): ShapelessCoproduct[T, R] =
      new ShapelessCoproduct[T, R](members, generic, this)
  }

  trait TypeValueExtractor {
    // can control the whether the symbol is allowed to be absent and treated as null
    def getTypeValue(tpe: NoSchema.ScalaType[_]): Option[Any]

  }

  trait UnionTypeValueCollector {
    def addTypeValue(tpe: NoSchema.ScalaType[_], value: Any): UnionTypeValueCollector
  }
}
