package org.datacrafts.noschema

import org.datacrafts.noschema.Context.LocalContext

object Context {

  sealed trait LocalContext[T] {
    def noSchema: NoSchema[T]
  }

  case class MemberVariable[T](symbol: Option[Symbol], noSchema: NoSchema[T])
    extends LocalContext[T]

  case class ContainerElement[T](noSchema: NoSchema[T])
    extends LocalContext[T]

  def root[T](node: NoSchema[T]): Context[T] =
    Context(MemberVariable(None, node), None)
}

case class Context[T](
  localContext: LocalContext[T], parentContext: Option[Context[_]]) {

  def noSchema: NoSchema[T] = localContext.noSchema

  override def toString: String =
    s"${parentContext.map(pc => s"${pc}").getOrElse("")}.${localContext}"

  def dependencyContext[D](context: LocalContext[D]): Context[D] =
    Context(localContext = context, parentContext = Some(this))
}
