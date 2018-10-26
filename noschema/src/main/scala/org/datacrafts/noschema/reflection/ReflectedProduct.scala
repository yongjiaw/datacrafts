package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.noschema.{Context, NoSchema, NoSchemaProduct, Operation}
import org.datacrafts.noschema.operator.ProductOperator.{SymbolCollector, SymbolExtractor}

class ReflectedProduct(
  runtimeType: ru.Type,
  fields: Map[ru.Symbol, Context.MemberVariable[Any]]
) extends NoSchemaProduct[Any](fields.values.toSeq) {

  import org.datacrafts.noschema.NoSchema._

  override lazy val scalaType: NoSchema.ScalaType[Any] =
    new NoSchema.ScalaType[Any](runtimeType.uniqueKey) {
      override lazy val tpe = runtimeType

      override def toString: String = s"RuntimeType[${uniqueKey}]"

      override def matchInput(input: Any): Option[Any] =
        if (input.getClass.getCanonicalName == tpe.typeSymbol.fullName) {
          Some(input)
        } else {
          Option.empty
        }
    }

  lazy val reflector = new TypeReflector(runtimeType)

  override def marshal(
    symbolExtractor: SymbolExtractor,
    operation: Operation[Any]
  ): Any = {
    val args =
      reflector.applyArgs.map {
        symbol =>
          val fieldContext = fields.get(symbol)
            .getOrElse(
              throw new Exception(s"symbol ${symbol} not found among fields ${fields.keys}"))
          operation
            .dependencyOperation(fieldContext)
            .marshal(symbolExtractor.getSymbolValue(fieldContext))
      }
    reflector.companionApply(args: _*)
  }

  override def unmarshal(
    input: Any,
    emptyCollector: SymbolCollector,
    operation: Operation[Any]
  ): SymbolCollector = {
    // TODO use fold to be functional
    var currentCollector = emptyCollector
    reflector.companionUnapply(input).map {
      case values =>
        for (i <- 0 until reflector.applyArgs.size) yield {
          val symbol = reflector.applyArgs(i)
          val fieldContext = fields.get(symbol)
            .getOrElse(
              throw new Exception(s"symbol ${symbol} not found among fields ${fields.keys}"))
          currentCollector = currentCollector.addSymbolValue(
            symbol = fieldContext,
            value = operation.dependencyOperation(fieldContext).unmarshal(values(i))
          )
        }
    }
    currentCollector
  }
}
