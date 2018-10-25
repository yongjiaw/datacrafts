package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}

import scala.util.{Failure, Success, Try}

class TypeReflector(tpe: ru.Type) {
  val rootMirror = ru.runtimeMirror(getClass.getClassLoader)
  val classMirror = rootMirror.reflectClass(tpe.typeSymbol.asClass)
  val companionSymbol = classMirror.symbol.companion
  val companionInstance = rootMirror.reflectModule(companionSymbol.asModule)
  val companionMirror = rootMirror.reflect(companionInstance.instance)

  def getCompanionMethodSymbol(methodName: String): ru.MethodSymbol = {
    companionSymbol.typeSignature.decl(ru.TermName(methodName)).asMethod
  }

  def companionApply(args: Any*): Any = {
    applyCompanionMethod("apply", args: _*)
  }

  def companionUnapply(value: Any): Option[Seq[Any]] = {
    applyCompanionMethod("unapply", value) match {
      case Some(result) =>
        val product = result.asInstanceOf[Product]
        Some((0 until product.productArity).map(product.productElement))
      case None => None
    }
  }

  def applyCompanionMethod(
    methodName: String,
    args: Any*
  ): Any = {
    companionMirror
      .reflectMethod(getCompanionMethodSymbol(methodName))
      .apply(args: _*)
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
