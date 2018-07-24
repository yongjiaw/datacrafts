package org.datacrafts.noschema

import org.datacrafts.noschema.Context.CoproductElement
import org.datacrafts.noschema.ShapelessCoproduct.{ShapelessCoproductAdapter, TypeValueExtractor, UnionTypeValueCollector}
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}

class ShapelessCoproduct[T, R <: Coproduct](
  override val dependencies: Seq[Context.CoproductElement[_]],
  generic: LabelledGeneric.Aux[T, R],
  shapeless: ShapelessCoproductAdapter[R],
  st: NoSchema.ScalaType[T]
) extends NoSchema[T](
  category = NoSchema.Category.CoProduct,
  nullable = true,
  dependencies = dependencies
)(st)
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

  trait Instances {
    implicit def shapelessCoproductRecursiveBuilder
    [K <: Symbol, V, L <: Coproduct](implicit
      headSymbol: Lazy[Witness.Aux[K]],
      head: Lazy[NoSchema[V]],
      tail: Lazy[ShapelessCoproductAdapter[L]],
      st: Lazy[NoSchema.ScalaType[V]]
    ): ShapelessCoproductAdapter[FieldType[K, V] :+: L] = {

      val headValueContext =
        Context.CoproductElement(headSymbol.value.value, NoSchema.getLazySchema(head)(st.value))

      new ShapelessCoproductAdapter[FieldType[K, V] :+: L](
        members = tail.value.members :+ headValueContext) {

        override def marshalCoproduct(
          typeValueExtractor: TypeValueExtractor, operation: Operation[_]
        ): FieldType[K, V] :+: L = {
          typeValueExtractor.getTypeValue(headValueContext) match {
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
            case Inl(headValue) => emptyUnion.addTypeValue(
              headValueContext,
              operation.dependencyOperation(headValueContext).operator.unmarshal(headValue)
            )
            case Inr(tailValue) => tail.value.unmarshalCoproduct(
              tailValue, emptyUnion, operation
            )
          }
        }
      }
    }

    implicit def shapelessCoproductRecursiveBuilderTerminator[K <: Symbol, V](
      implicit
      headSymbol: Lazy[Witness.Aux[K]],
      headValue: Lazy[NoSchema[V]],
      st: Lazy[NoSchema.ScalaType[V]]
    ): ShapelessCoproductAdapter[FieldType[K, V] :+: CNil] = {

      val headValueContext =
        Context.CoproductElement(
          headSymbol.value.value, NoSchema.getLazySchema(headValue)(st.value))

      new ShapelessCoproductAdapter[FieldType[K, V] :+: CNil](
        members = Seq(headValueContext)) {

        override def marshalCoproduct(
          typeExtractor: TypeValueExtractor, operation: Operation[_]): FieldType[K, V] :+: CNil = {
          typeExtractor.getTypeValue(headValueContext) match {
            case Some(value) =>
              Inl[FieldType[K, V], CNil](
                field[K](operation.dependencyOperation(headValueContext).operator.marshal(value)))
            case None =>
              throw new Exception(s"no value found for any type from $typeExtractor\n" +
                s"${operation.format()}")
          }
        }

        override def unmarshalCoproduct(
          coproduct: FieldType[K, V] :+: CNil,
          emptyUnion: UnionTypeValueCollector, operation: Operation[_]
        ): UnionTypeValueCollector = {
          coproduct match {
            case Inl(value) => emptyUnion.addTypeValue(
              headValueContext,
              operation.dependencyOperation(headValueContext).operator.unmarshal(value)
            )
            case _ => throw new Exception("impossible")
          }
        }
      }
    }

    implicit def shapelessCoproductBridging[T, R <: Coproduct](implicit
      generic: Lazy[LabelledGeneric.Aux[T, R]],
      shapeless: Lazy[ShapelessCoproductAdapter[R]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): NoSchema[T] = NoSchema.getOrElseCreateSchema(
      shapeless.value.composeWithGeneric(generic.value, st.value))(st.value)
  }

  abstract class ShapelessCoproductAdapter[R <: Coproduct](
    val members: Seq[Context.CoproductElement[_]]
  ) {
    def marshalCoproduct(typeExtractor: TypeValueExtractor, operation: Operation[_]): R

    def unmarshalCoproduct(
      coproduct: R, emptyUnion: UnionTypeValueCollector, operation: Operation[_]
    ): UnionTypeValueCollector

    def composeWithGeneric[T](
      generic: LabelledGeneric.Aux[T, R],
      st: NoSchema.ScalaType[T]): ShapelessCoproduct[T, R] =
      new ShapelessCoproduct[T, R](
        // filter the UnknownUnionField
        // this field will not produce value in unmarshaling,
        // and is not intended to take value in marshaling,
        // since schema evolution should never leave out already known types to unknown
        members.filter(clazz => !schemaClassFilter.contains(clazz.noSchema.scalaType.fullName)),
        generic, this, st)
  }

  trait TypeValueExtractor {
    // can control the whether the symbol is allowed to be absent and treated as null
    def getTypeValue(coproductElement: CoproductElement[_]): Option[Any]
  }

  trait UnionTypeValueCollector {
    def addTypeValue(coproductElement: CoproductElement[_], value: Any): UnionTypeValueCollector
  }
}
