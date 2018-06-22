package org.datacrafts.noschema.operator

import org.datacrafts.noschema.{Container, Context, NoSchema, Operation}
import org.datacrafts.noschema.Container.OptionContainer
import org.datacrafts.noschema.Context.ContainerElement
import org.datacrafts.noschema.Operation.Operator

trait ContainerOperator[T, C] extends Operation.Operator[C] {
  val element: ContainerElement[T]
  lazy val elementOperation: Operation[T] = operation.dependencyOperation(element)
}

object ContainerOperator {

  class OptionOperator[T] (
    override val element: ContainerElement[T],
    override val operation: Operation[Option[T]]
  ) extends ContainerOperator[T, Option[T]]{

    protected override def marshalNoneNull(input: Any): Option[T] = {

      // input could be Option[T] or just T
      val unwrapped = input match {
        case Some(value) => value
        case _ => input
      }
      Option(
        elementOperation.operator.marshal(unwrapped)
      )
    }

    protected override def unmarshalNoneNull(input: Option[T]): Any = {
      // unwrap the option
      input.map(elementOperation.operator.unmarshal)
        .getOrElse(null) // scalastyle:ignore
    }

    override def default: Option[Option[T]] = Some(None)
  }

  class SeqOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Seq[T]]
  ) extends ContainerOperator[T, Seq[T]] {

    protected override def marshalNoneNull(input: Any): Seq[T] = {
      input match {
        case value: Iterable[_] => value.map(elementOperation.operator.marshal).toSeq
        case None => throw new Exception(s"marshalling ${this} " +
          s"but input is not Iterable ${input.getClass}, ${input}")
      }
    }

    protected override def unmarshalNoneNull(input: Seq[T]): Any = {
      input.map(elementOperation.operator.unmarshal)
    }
  }

  class IterableOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Iterable[T]]
  ) extends ContainerOperator[T, Iterable[T]] {

    protected override def marshalNoneNull(input: Any): Iterable[T] = {
      input match {
        case value: Iterable[_] => value.map(elementOperation.operator.marshal).toSeq
        case None => throw new Exception(s"marshalling ${this} " +
          s"but input is not Iterable ${input.getClass}, ${input}")
      }
    }

    protected override def unmarshalNoneNull(input: Iterable[T]): Any = {
      input.map(elementOperation.operator.unmarshal)
    }
  }

  class MapOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Map[String, T]]
  ) extends ContainerOperator[T, Map[String, T]] {

    protected override def marshalNoneNull(input: Any): Map[String, T] = {
      input match {
        case value: Iterable[_] => value.map {
          case (k, v) => k.toString -> elementOperation.operator.marshal(v)
        }.toMap
        case None => throw new Exception(s"marshalling ${this} " +
          s"but input is not Iterable ${input.getClass}, ${input}")
      }
    }

    protected override def unmarshalNoneNull(input: Map[String, T]): Any = {
      input.map {
        case (k, v) => k -> elementOperation.operator.unmarshal(v)
      }
    }
  }
}
