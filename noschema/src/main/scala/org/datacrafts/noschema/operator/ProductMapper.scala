package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{NoSchemaProduct, Operation}
import org.datacrafts.noschema.Context.MemberVariable
import org.datacrafts.noschema.operator.ProductOperator.{ProductBuilder, SymbolCollector, SymbolExtractor}

class ProductMapper[T](
  override val operation: Operation[T],
  override val product: NoSchemaProduct[T],
  allowUnknownField: Boolean = false,
  allowAbsence: Boolean = true,
  includeNull: Boolean = true
) extends ProductOperator[T, Map[String, Any]] {

  import scala.collection.JavaConverters._
  protected def parseIterable(input: Any, iterable: Iterable[_]): SymbolExtractor = {
    new SymbolExtractor {
      private val map = collection.mutable.Map.empty[String, Any] ++
        iterable.map {
          case value => value match {
            case (k, v) => (s"$k" -> v)
            case _ => throw new Exception(s"$value does not match key value pair")
          }
        }

      override def removeSymbol(member: MemberVariable[_]): SymbolExtractor = {
        map -= member.symbol.name
        this
      }

      override def getSymbolValue(member: MemberVariable[_]): Any = {
        map.getOrElse(
          member.symbol.name,
          if (allowAbsence) {
            null //scalastyle:ignore
          } else {
            throw new Exception(
              s"${member.symbol.name} is absent for ${product.scalaType}: input=$input")
          }
        )
      }

      override def allSymbolsExtracted(): Unit = {
        if (!allowUnknownField && map.nonEmpty) {
          throw new Exception(
            s"there are unknown fields [${map.keySet.mkString(",")}] from input=$input" +
              s" for schema=${operation.context.noSchema.scalaType.fullName}")
        }
      }
    }
  }

  override protected def parse(input: Any): SymbolExtractor = input match {
    case javaMap: java.util.Map[_, _] => parseIterable(input, javaMap.asScala)
    case iterable: Iterable[_] => parseIterable(input, iterable)
    case _ => throw new Exception(s"input type ${input.getClass} is not Iterable[(_, _)]")
  }

  override protected def newProductBuilder(): ProductBuilder[Map[String, Any]] =
    new ProductBuilder[Map[String, Any]] {
      private val symbolMap = collection.mutable.Map.empty[Symbol, Any]

      override def build(): Map[String, Any] = symbolMap.map {
        case (k, v) => k.name -> v
      }.toSeq.sortBy(_._1).toMap

      override def addSymbolValue(member: MemberVariable[_],
        value: Any
      ): SymbolCollector = {
        if (includeNull || Option(value).isDefined) {
          symbolMap += member.symbol -> value
        }
        this
      }
    }

}
