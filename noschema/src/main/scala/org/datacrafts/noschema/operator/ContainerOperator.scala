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

  trait GenericCollectionOperator {
    Self: ContainerOperator[_, _] =>
    def iterableElementOperation: Operation[Any] = elementOperation.asInstanceOf[Operation[Any]]
    def marshalIterable(input: Any): Iterable[Any] = {
      input match {
        case value: Iterable[Any] => value.map(iterableElementOperation.marshal)
        case value: java.lang.Iterable[Any] =>
          value.asScala.map(iterableElementOperation.marshal)
        case _ => throw new Exception(
          s"marshalling input as Iterable " +
            s"but input type is not covered ${input.getClass}, ${input}")
      }
    }

    def unmarshalIterable(input: Iterable[Any]): Any = {
      input.map(iterableElementOperation.unmarshal)
    }
  }

  class SeqOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Seq[T]]
  ) extends ContainerOperator[T, Seq[T]] with GenericCollectionOperator {

    protected override def marshalNoneNull(input: Any): Seq[T] = {
      marshalIterable(input).asInstanceOf[Seq[T]]
    }

    protected override def unmarshalNoneNull(input: Seq[T]): Any = {
      unmarshalIterable(input)
    }
  }

  class SetOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Set[T]]
  ) extends ContainerOperator[T, Set[T]] with GenericCollectionOperator {

    protected override def marshalNoneNull(input: Any): Set[T] = {
      val elements = marshalIterable(input).asInstanceOf[Iterable[T]]
      val set = elements.toSet
      val size = elements.size
      if (size != set.size) {
        val duplicatedElements =
          for ((x, xs) <- elements.groupBy(x => x) if xs.size > 1) yield {x -> xs.size}

        throw new Exception(
          s"input size is ${size} but unique element size is ${set.size}, " +
            s"duplicates with count: ${duplicatedElements}. " +
            s"not safe to marshal input as set, marshal as Seq or convert input to Set first." +
            s"\ninput=${input}\noperation=${operation}"
        )
      }
      set
    }

    protected override def unmarshalNoneNull(input: Set[T]): Any = {
      unmarshalIterable(input)
    }
  }

  class IterableOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Iterable[T]]
  ) extends ContainerOperator[T, Iterable[T]] with GenericCollectionOperator {

    protected override def marshalNoneNull(input: Any): Iterable[T] = {
      marshalIterable(input).asInstanceOf[Iterable[T]]
    }

    protected override def unmarshalNoneNull(input: Iterable[T]): Any = {
      unmarshalIterable(input)
    }
  }

  class MapOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Map[String, T]]
  ) extends ContainerOperator[T, Map[String, T]] with GenericCollectionOperator {

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

  // due to NoSchema being invariant on the types
  // different Map implementations require different NoSchema class,
  // even the Maps are the same hierarchy
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

  // there is a trade off between having many specific operators vs a few general operators
  // the operation rule create operator instances and look them up in a map
  // having many specific operators leads to better performance due to reduced operator complexity
  // but it makes the operation rule more complex
  // comparing to the runtime lookup of operator instances, usually the extra conditional check
  // in a more complex rule does not have much overhead, so it's preferred to have a more general
  // operator whenever it applies
  class GeneralIterableOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Iterable[T]]
  ) extends ContainerOperator[T, Iterable[T]] with GenericCollectionOperator {

    protected override def marshalNoneNull(input: Any): Iterable[T] = {
      val iterable = marshalIterable(input)
      operation.context.noSchema.scalaType.fullName match {
        case "scala.collection.Iterable" => iterable
        case "scala.collection.Seq" => iterable.toSeq
        case name if name == "scala.collection.Set" || name == "scala.collection.immutable.Set" =>
          val set = iterable.toSet
          val size = iterable.size
          if (size != set.size) {
            val duplicatedElements =
              for ((x, xs) <- iterable.groupBy(x => x) if xs.size > 1) yield {x -> xs.size}

            throw new Exception(
              s"input size is ${size} but unique element size is ${set.size}, " +
                s"duplicates with count: ${duplicatedElements}. " +
                s"not safe to marshal input as set, marshal as Seq or convert input to Set first." +
                s"\ninput=${input}\noperation=${operation}"
            )
          }
          set
        case other =>
          throw new Exception(s"cannot convert iterable to ${other}")
      }
      // marshalIterable(input).asInstanceOf[Iterable[T]]
    }.asInstanceOf[Iterable[T]]

    protected override def unmarshalNoneNull(input: Iterable[T]): Any = {
      unmarshalIterable(input)
    }
  }

  /*
  class GeneralMapOperator[T](
    override val element: ContainerElement[T],
    override val operation: Operation[Iterable[(String, T)]]
  ) */
}
