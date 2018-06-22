package org.datacrafts.noschema.rule

import org.datacrafts.noschema._
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.operator.{PrimitiveOperator, ShapelessProductMapper}
import org.datacrafts.noschema.operator.ContainerOperator.{IterableOperator, MapOperator, OptionOperator, SeqOperator}

trait DefaultRule extends Operation.Rule {

  override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {

    operation.context.noSchema match {

      case _: Primitive[V] => new PrimitiveOperator[V](operation)

      case shapeless: ShapelessProduct[V, _] =>
        new ShapelessProductMapper[V](operation, shapeless)

      case option: OptionContainer[_] =>
        new OptionOperator[option.Elem](
          option.element, operation.asInstanceOf[Operation[Option[option.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case map: MapContainer[_] =>
        new MapOperator[map.Elem](
          map.element, operation.asInstanceOf[Operation[Map[String, map.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case seq: SeqContainer[_] =>
        new SeqOperator[seq.Elem](
          seq.element, operation.asInstanceOf[Operation[Seq[seq.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case iterable: IterableContainer[_] =>
        new IterableOperator[iterable.Elem](
          iterable.element, operation.asInstanceOf[Operation[Iterable[iterable.Elem]]])
          .asInstanceOf[Operation.Operator[V]]
    }
  }

}

object DefaultRule extends DefaultRule
