package org.datacrafts.noschema

import org.datacrafts.noschema.Context.LocalContext
import org.datacrafts.noschema.NoSchema.HasLazySchema

object Context {

  sealed trait LocalContext[T] {
    def noSchema: NoSchema[T]
  }

  case class MemberVariable[T](symbol: Symbol, lazySchema: HasLazySchema[T])
    extends LocalContext[T] {
    override lazy val noSchema: NoSchema[T] = lazySchema.lazySchema
  }


  case class ContainerElement[T](noSchema: NoSchema[T])
    extends LocalContext[T]

  case class CoproductElement[T](symbol: Symbol, noSchema: NoSchema[T])
    extends LocalContext[T]

  case class Root[T](noSchema: NoSchema[T]) extends LocalContext[T]

  def root[T](node: NoSchema[T]): Context[T] =
    Context(Root(node), None)
}

case class Context[T](
  localContext: LocalContext[T], parentContext: Option[Context[_]]) {

  def noSchema: NoSchema[T] = localContext.noSchema

  override def toString: String =
    s"${parentContext.map(pc => s"${pc}").getOrElse("")}.${localContext}"

  def dependencyContext[D](context: LocalContext[D]): Context[D] =
    Context(localContext = context, parentContext = Some(this))
}
