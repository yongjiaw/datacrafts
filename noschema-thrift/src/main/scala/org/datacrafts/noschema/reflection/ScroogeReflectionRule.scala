package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import com.twitter.scrooge.{ThriftStruct, ThriftUnion}
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Context, NoSchema}

trait ScroogeReflectionRule extends NoSchemaReflector.ReflectionRule with Slf4jLogging.Default {
  import ru.typeOf

  override def reflect(tpe: ru.Type): NoSchema[Any] = {
    val reflector = TypeReflector(tpe)
    if (tpe < typeOf[ThriftUnion]) { // union type extends both ThriftUnion and ThriftStruct
      if (reflector.subclasses.nonEmpty) { // coproduct
        logInfo(
          s"${tpe.typeSymbol.fullName} is coproduct of (${reflector.subclasses})")
        new ReflectedCoproduct(
          tpe,
          members = (
            for (
              symbol <- reflector.subclasses
              if (symbol.name.toString != "UnknownUnionField")
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
    else if (tpe < typeOf[ThriftStruct]) {
      logInfo(s"${tpe.typeSymbol.fullName} is thrift struct")
      new ReflectedProduct(
        tpe,
        fields = reflector.applyArgs.map {
          s =>
            val symbolName = s.name.toString
            val symbolType =
              reflector.caseMemberTypeMap.get(symbolName).getOrElse(
                s.typeSignature
              )
            s -> Context.MemberVariable(
              Symbol(symbolName),

              getOrCreateLazySchema(
                symbolType,
                reflect(symbolType)
              )
            )
        }.toMap
      )
    } else {
      logInfo(s"${tpe.typeSymbol.fullName} is not thrift, use default rule")
      super.reflect(tpe)
    }

  }
}
