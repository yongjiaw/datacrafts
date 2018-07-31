package org.datacrafts.noschema.avro

import scala.collection.JavaConverters._

import org.datacrafts.noschema.Operation
import org.datacrafts.noschema.Context.ContainerElement
import org.datacrafts.noschema.operator.ContainerOperator._

object AvroContainerOperator {

  class AvroMapOperator[T](
    override val element: ContainerElement[T],
    override val operation: AvroOperation[Map[String, T]]
  ) extends MapOperator[T](element, operation) {
    override lazy val elementOperation: AvroOperation[T] = operation.dependencyOperation(element)

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

    protected override def unmarshalNoneNull(input: Map[String, T]): Any =
      input.map {
        case (k, v) => k -> elementOperation.unmarshal(v)
      }.asJava
  }

  class AvroMapOperator2[T](
    override val element: ContainerElement[T],
    override val operation: AvroOperation[scala.collection.Map[String, T]]
  ) extends MapOperator2[T](element, operation) {
    override lazy val elementOperation: AvroOperation[T] = operation.dependencyOperation(element)

    protected override def unmarshalNoneNull(input: scala.collection.Map[String, T]): Any =
      input.map {
        case (k, v) => k -> elementOperation.unmarshal(v)
      }.asJava
  }

  class AvroSeqOperator[T](
    override val element: ContainerElement[T],
    override val operation: AvroOperation[Seq[T]]
  ) extends SeqOperator(element, operation) {
    override lazy val elementOperation: AvroOperation[T] = operation.dependencyOperation(element)
    protected override def unmarshalNoneNull(input: Seq[T]): Any =
      input.map(elementOperation.unmarshal).asJava
  }

  class AvroIterableOperator[T](
    override val element: ContainerElement[T],
    override val operation: AvroOperation[Iterable[T]]
  ) extends IterableOperator(element, operation) {
    override lazy val elementOperation: AvroOperation[T] = operation.dependencyOperation(element)
    protected override def unmarshalNoneNull(input: Iterable[T]): Any =
      input.map(elementOperation.unmarshal).asJava
  }

  class AvroOptionOperator[T](
    override val element: ContainerElement[T],
    override val operation: AvroOperation[Option[T]]
  ) extends OptionOperator(element, operation) {
    override lazy val elementOperation: AvroOperation[T] = operation.dependencyOperation(element)
  }

}
