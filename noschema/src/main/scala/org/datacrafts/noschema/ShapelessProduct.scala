package org.datacrafts.noschema

import org.datacrafts.noschema.ShapelessCoproduct.{ShapelessCoproductAdapter, TypeValueExtractor, UnionTypeValueCollector}
import org.datacrafts.noschema.ShapelessProduct.{ShapelessProductAdapter, SymbolCollector, SymbolExtractor}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}

class ShapelessProduct[T : NoSchema.ScalaType, R <: HList](
  dependencies: Seq[Context.MemberVariable[_]],
  generic: LabelledGeneric.Aux[T, R],
  shapeless: ShapelessProductAdapter[R]
) extends NoSchema[T](
  category = NoSchema.Category.Product,
  nullable = true,
  dependencies = dependencies
)
{
  def marshal(symbolExtractor: SymbolExtractor, operation: Operation[T]): T = {
    generic.from(shapeless.marshalHList(symbolExtractor, operation))
  }

  def unmarshal(input: T, emptyCollector: SymbolCollector, operation: Operation[T]
  ): SymbolCollector = {
    shapeless.unmarshalHList(generic.to(input), emptyCollector, operation)
  }
}

object ShapelessProduct {

  trait FallbackImplicits {

    // this helps pass compile time check, and will throw runtime error
    // if certain type transformer is missing
    implicit def lowPriorityFallBackImplicit[K <: Symbol, V: NoSchema.ScalaType, L <: HList]
    (implicit
      w: Witness.Aux[K],
      l: ShapelessProductAdapter[L]
    ): ShapelessProductAdapter[FieldType[K, V] :: L] = {
      throw new Exception(s"field ${w.value.name} of ${implicitly[NoSchema.ScalaType[V]]} " +
        s"does not have NoSchema in scope.")
    }

  }

  trait Instances extends FallbackImplicits {

    implicit val hNilNode = new ShapelessProductAdapter[HNil](Seq.empty) {
      override def marshalHList(symbolExtractor: SymbolExtractor, operation: Operation[_]) = {
        symbolExtractor.allSymbolsExtracted()
        HNil
      }

      override def unmarshalHList(hList: HNil,
        emptyBuilder: SymbolCollector, operation: Operation[_]
      ): SymbolCollector = {
        emptyBuilder
      }
    }

    implicit def shapelessProductRecursiveBuilder[K <: Symbol, V: NoSchema.ScalaType, L <: HList](
      implicit
      headSymbol: Witness.Aux[K],
      headValue: Lazy[NoSchema[V]],
      tail: Lazy[ShapelessProductAdapter[L]]
    ): ShapelessProductAdapter[FieldType[K, V] :: L] = {

      val headValueContext =
        Context.MemberVariable(headSymbol.value, NoSchema.getLazySchema(headValue))

      new ShapelessProductAdapter[FieldType[K, V] :: L](
        members = tail.value.members :+ headValueContext) {

        override def marshalHList(
          symbolExtractor: SymbolExtractor, operation: Operation[_]): FieldType[K, V] :: L = {
          field[K](
            operation.dependencyOperation(headValueContext).operator
                          .marshal(symbolExtractor.getSymbolValue(headSymbol.value))
          ) :: tail.value.marshalHList(symbolExtractor.removeSymbol(headSymbol.value), operation)
        }

        override def unmarshalHList(
          hList: FieldType[K, V] :: L, emptyBuilder: SymbolCollector, operation: Operation[_]
        ): SymbolCollector = {
          tail.value.unmarshalHList(hList.tail, emptyBuilder, operation)
            .addSymbolValue(
              symbol = headSymbol.value,
              value = operation.dependencyOperation(headValueContext)
                .operator.unmarshal(hList.head)
            )
        }
      }
    }

    implicit def shapelessProductBridging[T: NoSchema.ScalaType, R <: HList](implicit
      generic: LabelledGeneric.Aux[T, R],
      shapeless: Lazy[ShapelessProductAdapter[R]]
    ): NoSchema[T] = shapeless.value.composeWithGeneric(generic)

  }

  trait SymbolExtractor {
    def removeSymbol(symbol: Symbol): SymbolExtractor

    // can control the whether the symbol is allowed to be absent and treated as null
    def getSymbolValue(symbol: Symbol): Any

    def allSymbolsExtracted(): Unit
  }

  trait SymbolCollector {

    // add symbol value pairs disassembled from the structured class
    // can control the behavior when the symbol has null or empty value
    def addSymbolValue(symbol: Symbol, value: Any): SymbolCollector

  }

  /**
    * 1. can perform recursive construction/destruction of HList
    * 2. can compose Product type with labelledGeneric bridging
    */
  abstract class ShapelessProductAdapter[R <: HList](
    val members: Seq[Context.MemberVariable[_]]
  ) {

    def marshalHList(symbolExtractor: SymbolExtractor, operation: Operation[_]): R

    def unmarshalHList(hList: R, emptyCollector: SymbolCollector, operation: Operation[_]
    ): SymbolCollector

    def composeWithGeneric[T: NoSchema.ScalaType](
      generic: LabelledGeneric.Aux[T, R]): NoSchema[T] =
      new ShapelessProduct(members, generic, this)
  }
}
