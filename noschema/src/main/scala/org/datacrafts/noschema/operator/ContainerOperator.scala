package org.datacrafts.noschema.operator

import scala.collection.JavaConverters._

import org.datacrafts.noschema.Context.ContainerElement
import org.datacrafts.noschema.Operation

trait ContainerOperator[T, C] extends Operation.Operator[C] {
  val element: ContainerElement[T]
  lazy val elementOperation: Operation[T] = operation.dependencyOperation(element)
  type Elem = T
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
        elementOperation.marshal(unwrapped)
      )
    }

    protected override def unmarshalNoneNull(input: Option[T]): Any = {
      // unwrap the option
      input.map(elementOperation.unmarshal)
        .getOrElse(null) // scalastyle:ignore
    }

    final override def default: Option[Option[T]] = {
      Option(elementOperation.operator.default)
    }
  }

  class SeqOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Seq[T]]
  ) extends ContainerOperator[T, Seq[T]] {

    protected override def marshalNoneNull(input: Any): Seq[T] = {
      input match {
        case value: Iterable[_] => value.map(elementOperation.marshal).toSeq
        case value: java.lang.Iterable[_] =>
          value.asScala.map(elementOperation.marshal).toSeq
        case _ => throw new Exception(
          s"marshalling ${operation.context.noSchema.scalaType.uniqueKey} " +
            s"but input type is not covered ${input.getClass}, ${input}")
      }
    }

    protected override def unmarshalNoneNull(input: Seq[T]): Any = {
      input.map(elementOperation.unmarshal)
    }
  }

  class IterableOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Iterable[T]]
  ) extends ContainerOperator[T, Iterable[T]] {

    protected override def marshalNoneNull(input: Any): Iterable[T] = {
      input match {
        case value: Iterable[_] => value.map(elementOperation.marshal).toSeq
        case value: java.lang.Iterable[_] =>
          value.asScala.map(elementOperation.marshal).toSeq
        case _ => throw new Exception(
          s"marshalling ${operation.context.noSchema.scalaType.uniqueKey} " +
            s"but input type is not covered ${input.getClass}, ${input}")
      }
    }

    protected override def unmarshalNoneNull(input: Iterable[T]): Any = {
      input.map(elementOperation.unmarshal)
    }
  }

  class MapOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Map[String, T]]
  ) extends ContainerOperator[T, Map[String, T]] {

    protected override def marshalNoneNull(input: Any): Map[String, T] = {
      input match {
        case value: Iterable[_] => value.map {
          case (k, v) => s"$k" -> elementOperation.marshal(v)
        }.toMap
        case value: java.util.Map[_, _] => value.asScala.map {
          case (k, v) => s"$k" -> elementOperation.marshal(v)
        }.toMap
        case _ => throw new Exception(
          s"marshalling ${operation.context.noSchema.scalaType.uniqueKey} " +
          s"but input type is not covered ${input.getClass}, ${input}")
      }
    }

    protected override def unmarshalNoneNull(input: Map[String, T]): Any = {
      input.map {
        case (k, v) => k -> elementOperation.unmarshal(v)
      }
    }
  }

  class MapOperator2[T](
    override val element: ContainerElement[T],
    override val operation: Operation[scala.collection.Map[String, T]]
  ) extends ContainerOperator[T, scala.collection.Map[String, T]] {

    protected override def marshalNoneNull(input: Any): Map[String, T] = {
      input match {
        case value: Iterable[_] => value.map {
          case (k, v) => s"$k" -> elementOperation.marshal(v)
        }.toMap
        case value: java.util.Map[_, _] => value.asScala.map {
          case (k, v) => s"$k" -> elementOperation.marshal(v)
        }.toMap
        case _ => throw new Exception(
          s"marshalling ${operation.context.noSchema.scalaType.uniqueKey} " +
            s"but input type is not covered ${input.getClass}, ${input}")
      }
    }

    protected override def unmarshalNoneNull(input: scala.collection.Map[String, T]): Any = {
      input.map {
        case (k, v) => k -> elementOperation.unmarshal(v)
      }
    }
  }
}
