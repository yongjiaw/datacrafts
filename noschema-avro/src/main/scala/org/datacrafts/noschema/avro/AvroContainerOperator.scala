package org.datacrafts.noschema.avro

import scala.collection.JavaConverters._

import org.datacrafts.noschema.Context.ContainerElement
import org.datacrafts.noschema.operator.ContainerOperator._

object AvroContainerOperator {

  class AvroMapOperator[T](
    override val element: ContainerElement[T],
    override val operation: AvroOperation[Iterable[(String, T)]]
  ) extends GeneralMapOperator[T](element, operation) {
    override lazy val elementOperation: AvroOperation[T] = operation.dependencyOperation(element)

    protected override def marshalNoneNull(input: Any): Iterable[(String, T)] = {
      super.marshalNoneNull(input)
    }
    protected override def unmarshalNoneNull(input: Iterable[(String, T)]): Any =
      input.map {
        case (k, v) => k -> elementOperation.unmarshal(v)
      }.toMap.asJava
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
  ) extends GeneralIterableOperator(element, operation) {
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
