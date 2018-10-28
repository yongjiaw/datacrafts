package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import org.datacrafts.noschema.{Context, NoSchema, NoSchemaProduct, Operation}
import org.datacrafts.noschema.operator.ProductOperator.{SymbolCollector, SymbolExtractor}
import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectedScalaType

class ReflectedProduct(
  runtimeType: ru.Type,
  fields: Map[ru.Symbol, Context.MemberVariable[Any]]
) extends NoSchemaProduct[Any](fields.values.toSeq) {

  override lazy val scalaType = new ReflectedScalaType(runtimeType)

  lazy val reflector = TypeReflector(runtimeType)

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

    // case object just return the instance by reflection
    if (!reflector.companionSymbol.isModule) {
      reflector.moduleMirror.instance
    } else {
      reflector.companionApply(args: _*)
    }
  }

  override def unmarshal(
    input: Any,
    emptyCollector: SymbolCollector,
    operation: Operation[Any]
  ): SymbolCollector = {
    // TODO use fold to be functional
    var currentCollector = emptyCollector
    // case object nothing to add
    if (!reflector.companionSymbol.isModule) {
      // nothing to add
    } else {
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
    }
    currentCollector
  }
}
