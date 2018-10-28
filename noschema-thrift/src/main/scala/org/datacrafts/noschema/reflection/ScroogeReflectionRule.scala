package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import com.twitter.scrooge.{ThriftStruct, ThriftUnion}
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Context, NoSchema}

trait ScroogeReflectionRule extends NoSchemaReflector.ReflectionRule with Slf4jLogging.Default {

  protected def isThriftUnion(tpe: ru.Type): Boolean = {
    // val result = tpe.dealiasedType.typeSymbol.typeSignature < typeOf[ThriftUnion]
    val result = tpe.dealiasedType.typeSymbol.typeSignature.toString.contains("with com.twitter.scrooge.ThriftUnion")
    logDebug(s"${tpe.dealiasedType.typeSymbol.typeSignature} isThriftUnion=$result")
    result
  }

  protected def isThrift(tpe: ru.Type): Boolean = {
    // tpe < typeOf[ThriftStruct]
    val result = tpe.dealiasedType.typeSymbol.typeSignature.toString.contains("with com.twitter.scrooge.ThriftStruct")
    logDebug(s"${tpe.dealiasedType.typeSymbol.typeSignature} isThriftStruct=$result")
    result
  }

  override protected def coproductFilter(symbol: ru.Symbol): Boolean = {
    val result = symbol.name.toString != "UnknownUnionField"
    logDebug(s"filtering ${symbol}, result=$result")
    result
  }

  override protected def productFilter(symbol: ru.Symbol): Boolean = true

  override def reflect(tpe: ru.Type): NoSchema[Any] = {
    val reflector = TypeReflector(tpe)
    if (isThriftUnion(tpe)) { // union type extends both ThriftUnion and ThriftStruct
      if (reflector.subclasses.nonEmpty) { // coproduct
        logInfo(
          s"${tpe.typeSymbol.fullName} is thrift coproduct of (${reflector.subclasses})")
        new ReflectedCoproduct(
          tpe,
          members = (
            for (
              symbol <- reflector.subclasses if coproductFilter(symbol)
            ) yield {
              val symbolName = symbol.name.toString
              val subClassReflector = new TypeReflector(symbol.typeSignature)
              if (subClassReflector.caseAccessors.size == 0) { // enum is case object
                ReflectedCoproduct.Member(
                  symbol,
                  None,
                  Context.CoproductElement(
                    Symbol(symbolName),
                    reflect(symbol.typeSignature.dealias)
                  )
                )
              }
              else if (subClassReflector.caseAccessors.size == 1) {
                // everything else is wrapped in case class
                val wrappedMember = subClassReflector.caseAccessors(0)
                ReflectedCoproduct.Member(
                  wrappedMember,
                  Some(symbol),
                  Context.CoproductElement(
                    Symbol(symbolName),
                    reflect(wrappedMember.typeSignature.dealias)
                  )
                )
              }
              else {
                throw new Exception(
                  s"ThriftUnion's subclass should not have more than 1 case accessors:" +
                    s"${symbolName}(${subClassReflector.caseAccessors})"
                )
              }

            }).toSeq
        )
      }
      else {
        throw new Exception(s"${tpe.typeSymbol.fullName} not recognized")
      }
    }
    else if (isThrift(tpe)) {
      logInfo(s"${tpe.typeSymbol.fullName} is thrift struct")
      new ReflectedProduct(
        tpe,
        fields = (
          for (
            s <- reflector.applyArgs if productFilter(s)
          ) yield {
            val symbolName = s.name.toString
            val symbolType = s.typeSignature
            s -> Context.MemberVariable(
              Symbol(symbolName),
              getOrCreateLazySchema(
                symbolType,
                reflect(symbolType.dealias)
              )
            )
        }).toMap
      )
    } else {
      logInfo(s"${tpe.typeSymbol.fullName} is not thrift, use default rule")
      super.reflect(tpe)
    }

  }
}
