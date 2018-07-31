package org.datacrafts.noschema

import org.datacrafts.noschema.Context.MemberVariable
import org.datacrafts.noschema.ShapelessProduct.{ShapelessProductAdapter, SymbolCollector, SymbolExtractor}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{field, FieldType}

class ShapelessProduct[T, R <: HList](
  override val dependencies: Seq[Context.MemberVariable[_]],
  generic: LabelledGeneric.Aux[T, R],
  shapeless: ShapelessProductAdapter[R],
  st: NoSchema.ScalaType[T]
) extends NoSchema[T](
  category = NoSchema.Category.Product,
  nullable = true,
  dependencies = dependencies
)(st)
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
    implicit def lowPriorityFallBackImplicit[K <: Symbol, V, L <: HList]
    (implicit
      w: Lazy[Witness.Aux[K]],
      l: Lazy[ShapelessProductAdapter[L]],
      st: Lazy[NoSchema.ScalaType[V]]
    ): ShapelessProductAdapter[FieldType[K, V] :: L] = {
      throw new Exception(s"field ${w.value.value.name} of ${st.value} " +
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

    implicit def shapelessProductRecursiveBuilder[K <: Symbol, V, L <: HList](
      implicit
      headSymbol: Lazy[Witness.Aux[K]],
      headValue: Lazy[NoSchema[V]],
      tail: Lazy[ShapelessProductAdapter[L]],
      st: Lazy[NoSchema.ScalaType[V]]
    ): ShapelessProductAdapter[FieldType[K, V] :: L] = {

      val headValueContext =
        Context.MemberVariable(headSymbol.value.value, NoSchema.getLazySchema(headValue)(st.value))

      new ShapelessProductAdapter[FieldType[K, V] :: L](
        members = tail.value.members :+ headValueContext) {

        override def marshalHList(
          symbolExtractor: SymbolExtractor, operation: Operation[_]): FieldType[K, V] :: L = {
          field[K](
            operation.dependencyOperation(headValueContext)
                          .marshal(symbolExtractor.getSymbolValue(headValueContext))
          ) :: tail.value.marshalHList(
            symbolExtractor.removeSymbol(headValueContext), operation)
        }

        override def unmarshalHList(
          hList: FieldType[K, V] :: L, emptyBuilder: SymbolCollector, operation: Operation[_]
        ): SymbolCollector = {
          tail.value.unmarshalHList(hList.tail, emptyBuilder, operation)
            .addSymbolValue(
              symbol = headValueContext,
              value = operation.dependencyOperation(headValueContext)
                .unmarshal(hList.head)
            )
        }
      }
    }

    implicit def shapelessProductBridging[T, R <: HList](implicit
      generic: Lazy[LabelledGeneric.Aux[T, R]],
      shapeless: Lazy[ShapelessProductAdapter[R]],
      st: Lazy[NoSchema.ScalaType[T]]
    ): NoSchema[T] = NoSchema.getOrElseCreateSchema(
      shapeless.value.composeWithGeneric(generic.value, st.value))(st.value)

  }

  trait SymbolExtractor {
    def removeSymbol(symbol: MemberVariable[_]): SymbolExtractor

    // can control the whether the symbol is allowed to be absent and treated as null
    def getSymbolValue(symbol: MemberVariable[_]): Any

    def allSymbolsExtracted(): Unit
  }

  trait SymbolCollector {

    // add symbol value pairs disassembled from the structured class
    // can control the behavior when the symbol has null or empty value
    def addSymbolValue(symbol: MemberVariable[_], value: Any): SymbolCollector

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

    def composeWithGeneric[T](
      generic: LabelledGeneric.Aux[T, R],
      st: NoSchema.ScalaType[T]): NoSchema[T] =
      new ShapelessProduct(members, generic, this, st)
  }
}
