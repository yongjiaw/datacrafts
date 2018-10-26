package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success, Try}

import org.datacrafts.logging.Slf4jLogging

class TypeReflector(val tpe: ru.Type) extends Slf4jLogging.Default {

  import org.datacrafts.noschema.NoSchema._
  logInfo(s"constructing reflector for ${tpe.uniqueKey}")

  lazy val rootMirror = ru.runtimeMirror(getClass.getClassLoader)
  lazy val classMirror = rootMirror.reflectClass(tpe.typeSymbol.asClass)
  lazy val companionSymbol = classMirror.symbol.companion
  lazy val companionInstance = rootMirror.reflectModule(companionSymbol.asModule)
  lazy val companionMirror = rootMirror.reflect(companionInstance.instance)

  lazy val fullName: String = tpe.typeSymbol.fullName
  def getCompanionMethodSymbol(methodName: String): ru.MethodSymbol = {
    companionSymbol.typeSignature.decl(ru.TermName(methodName)).asMethod
  }

  lazy val subclasses: Set[ru.Symbol] = {
    tpe.typeSymbol.asClass.knownDirectSubclasses
  }

  lazy val caseAccessors: Seq[ru.MethodSymbol] = {
    tpe.typeSymbol.typeSignature.members.collect {
      case m: ru.MethodSymbol if m.isCaseAccessor => m
    }.toSeq
  }

  lazy val caseMemberTypeMap: Map[String, ru.Type] = caseAccessors.map {
    accessor =>
      accessor.asTerm.name.toString -> accessor.typeSignatureIn(tpe).finalResultType
  }.toMap

  def companionApply(args: Any*): Any = {
    logDebug(s"calling ${fullName}.apply: input=${args}")
    applyCompanionMethod("apply", args: _*)
  }

  def companionUnapply(value: Any): Option[Seq[Any]] = {
    applyCompanionMethod("unapply", value) match {
      case Some(result) =>
        logDebug(s"${fullName}.unapply: input=${value}, output=${result}")
        if (result.isInstanceOf[Product]) { // more than 1 values
          val product = result.asInstanceOf[Product]
          Some((0 until product.productArity).map(product.productElement))
        }
        else { // 1 value
          Some(Seq(result))
        }
      case None => None
    }
  }

  def applyCompanionMethod(
    methodName: String,
    args: Any*
  ): Any = {
    Try(companionMirror
      .reflectMethod(getCompanionMethodSymbol(methodName))
      .apply(args: _*)) match {
      case Success(result) => result
      case Failure(f) => throw new Exception(s"failed ${fullName}.apply(${args})", f)
    }
  }

  // the type signature of the symbol can be a type parameter
  // right now it can only be resolved based on position in the applyArg list and type arg list
  // applyArg list ordering and type arg list ordering may be incosistent like:
  // case lass SomeClass[T1, T2](a: T2, b: T1)
  // there does not seem to be a way to directly get
  class SymbolWithFinalType(val symbol: ru.Symbol, val finalType: ru.Type) {

  }

  lazy val applyArgs: Seq[ru.Symbol] = Try(getCompanionMethodSymbol("apply")) match {
    case Success(applyMethod) =>
      val paramList = applyMethod.paramLists
      if (paramList.size > 1) {
        throw new Exception(s"multiple paramLists(${paramList.size}):${paramList}")
      }
      paramList(0)
    case Failure(f) =>
      throw new Exception(s"failed to reflect apply method for ${tpe.typeSymbol.fullName}")
  }

  def getClassMethodSymbol(methodName: String): Option[ru.MethodSymbol] = {
    classMirror.symbol.typeSignature.members.collectFirst {
      case m: ru.MethodSymbol if m.name.toString == methodName => m
    }
  }

  // it's expensive to invoked reflection on every instance
  def instanceMirror(obj: Any): ru.InstanceMirror = rootMirror.reflect(obj)

  def applyInstanceMethod(obj: Any,
    methodName: String,
    args: Any*
  ): Any = {
    instanceMirror(obj)
      .reflectMethod(getClassMethodSymbol(methodName).get)
      .apply(args: _*)
  }
}