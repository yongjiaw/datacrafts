package org.datacrafts.noschema.reflection

import java.net.URLClassLoader

import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success, Try}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.NoSchema.{TypeTagConverter, TypeUniqueKey}

object TypeReflector {
  private val _reflectors = collection.mutable.Map.empty[TypeUniqueKey, TypeReflector]
  def apply(tpe: ru.Type): TypeReflector = _reflectors.synchronized {
    _reflectors.getOrElseUpdate(
      tpe.uniqueKey,
      new TypeReflector(tpe)
    )
  }

  var classLoader = Thread.currentThread().getContextClassLoader
}

class TypeReflector(val originalType: ru.Type) extends Slf4jLogging.Default {

  val dealiasedType = {
    var current = originalType
    var next = originalType.dealias
    while (current != next) {
      current = next
      next = next.dealias
    }
    current
  }
  import org.datacrafts.noschema.NoSchema._
  logInfo(s"constructing reflector for ${dealiasedType.uniqueKey}=${dealiasedType.typeSymbol}")
  lazy val fullName: String = dealiasedType.typeSymbol.fullName

  lazy val classLoader = TypeReflector.classLoader
  lazy val rootMirror = ru.runtimeMirror(classLoader)

  lazy val classMirror = rootMirror.reflectClass(dealiasedType.typeSymbol.asClass)

  lazy val classLoaderMessage = {
    classLoader match {
      case cl: URLClassLoader =>
        s"NoSchema Reflection ClassLoader URLs ${cl.getURLs.toSeq}"
      case _ =>
        s"NoSchema Reflection ClassLoader is ${TypeReflector.classLoader}"
    }
  }
  // module is for general object, for case object it does not have companion
  lazy val moduleSymbol = {
    Try(rootMirror.staticModule(fullName)) match {
      case Success(symbol) => symbol
      case Failure(f) =>
        throw new Exception(
          s"failed to get static module: ${classLoaderMessage}", f)
    }
  }
  lazy val moduleMirror = rootMirror.reflectModule(moduleSymbol)

  // companion object is a specific type of module
  lazy val companionSymbol = classMirror.symbol.companion
  lazy val companionModuleMirror = moduleMirror // rootMirror.reflectModule(companionSymbol.asModule)
  lazy val companionInstanceMirror = rootMirror.reflect(companionModuleMirror.instance)

  def getCompanionMethodSymbol(methodName: String): ru.MethodSymbol = {
    companionSymbol.typeSignature.decl(ru.TermName(methodName)).asMethod
  }

  lazy val subclasses: Set[ru.Symbol] = {
    dealiasedType.typeSymbol.asClass.knownDirectSubclasses
  }

  lazy val caseAccessors: Seq[ru.MethodSymbol] = {
    dealiasedType.typeSymbol.typeSignature.members.collect {
      case m: ru.MethodSymbol if m.isCaseAccessor => m
    }.toSeq
  }

  lazy val caseMemberTypeMap: Map[String, ru.Type] = caseAccessors.map {
    accessor =>
      accessor.asTerm.name.toString -> accessor.typeSignatureIn(dealiasedType).finalResultType
  }.toMap

  def < (other: TypeReflector): Boolean = {
    dealiasedType <:< other.dealiasedType
  }

  def companionApply(args: Any*): Any = {
    logDebug(s"calling ${fullName}.apply: input=${args}")
    applyMethod(applyMethodMirror, args: _*)
  }

  def companionUnapply(value: Any): Option[Seq[Any]] = {
    applyMethod(unapplyMethodMirror, value) match {
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

  lazy val applyMethodMirror = companionInstanceMirror
    .reflectMethod(getCompanionMethodSymbol("apply"))

  lazy val unapplyMethodMirror = companionInstanceMirror
    .reflectMethod(getCompanionMethodSymbol("unapply"))

  def applyMethod(
    methodMirror: ru.MethodMirror,
    args: Any*
  ): Any = {
    Try(methodMirror.apply(args: _*)) match {
      case Success(result) => result
      case Failure(f) =>
        val currentThread = Thread.currentThread()
        throw new Exception(
          s"failed ${fullName}.${methodMirror.symbol}(${s"${args}".take(100)}). " +
        s"classSymbol=${classMirror.symbol}, currentThread=${currentThread.getName}", f)
    }
  }

  lazy val applyArgs: Seq[ru.Symbol] = Try(getCompanionMethodSymbol("apply")) match {
    case Success(applyMethod) =>
      val paramList = applyMethod.paramLists
      if (paramList.size > 1) {
        throw new Exception(s"multiple paramLists(${paramList.size}):${paramList}")
      }
      paramList(0)
    case Failure(f) =>
      // throw new Exception(s"failed to reflect apply method for ${tpe.typeSymbol.fullName}")
      Seq.empty
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
