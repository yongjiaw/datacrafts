package org.datacrafts.noschema

import org.datacrafts.noschema.ShapelessProduct.{ShapelessAdapter, SymbolCollector, SymbolExtractor}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}

class ShapelessProduct[T : NoSchema.Type, R <: HList](
  dependencies: Seq[Context.MemberVariable[_]],
  generic: LabelledGeneric.Aux[T, R],
  shapeless: ShapelessAdapter[R]
) extends NoSchema[T](
  category = NoSchema.Category.Struct,
  nullable = true
)
{
  def marshal(symbolExtractor: SymbolExtractor, operation: Operation[T]): T = {
    generic.from(shapeless.marshalHList(symbolExtractor, operation))
  }

  def unmarshal(input: T, emptyBuilder: SymbolCollector, operation: Operation[T]
  ): SymbolCollector = {
    shapeless.unmarshalHList(generic.to(input), emptyBuilder, operation)
  }
}

object ShapelessProduct {

  trait FallbackImplicits {

    // this helps pass compile time check, and will throw runtime error
    // if certain type transformer is missing
    implicit def lowPriorityFallBackImplicit[K <: Symbol, V: NoSchema.Type, L <: HList](implicit
      w: Witness.Aux[K],
      l: ShapelessAdapter[L]
    ): ShapelessAdapter[FieldType[K, V] :: L] = {
      throw new Exception(s"field ${w.value.name} of ${implicitly[NoSchema.Type[V]]} " +
        s"does not have NoSchema in scope.")
    }

  }

  trait Implicits extends FallbackImplicits {

    implicit val hNilNode = new ShapelessAdapter[HNil](Seq.empty) {
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

    implicit def shapelessRecursiveBuilder[K <: Symbol, V, L <: HList](implicit
      headSymbol: Witness.Aux[K],
      headValue: Lazy[NoSchema[V]],
      tail: Lazy[ShapelessAdapter[L]]
    ): ShapelessAdapter[FieldType[K, V] :: L] = {

      val headValueContext =
        Context.MemberVariable(Some(headSymbol.value), headValue.value)

      new ShapelessAdapter[FieldType[K, V] :: L](
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

    implicit def shapelessBridging[T: NoSchema.Type, R <: HList](implicit
      generic: LabelledGeneric.Aux[T, R],
      shapeless: Lazy[ShapelessAdapter[R]]
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
  abstract class ShapelessAdapter[R <: HList](
    val members: Seq[Context.MemberVariable[_]]
  ) {

    def marshalHList(symbolExtractor: SymbolExtractor, operation: Operation[_]): R

    def unmarshalHList(hList: R, emptyBuilder: SymbolCollector, operation: Operation[_]
    ): SymbolCollector

    def composeWithGeneric[T: NoSchema.Type](
      generic: LabelledGeneric.Aux[T, R]): NoSchema[T] =
      new ShapelessProduct(members, generic, this)
  }
}
