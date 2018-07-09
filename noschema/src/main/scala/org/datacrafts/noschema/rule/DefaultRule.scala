package org.datacrafts.noschema.rule

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema._
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.operator.{PrimitiveOperator, ShapelessCoproductTupler, ShapelessProductMapper}
import org.datacrafts.noschema.operator.ContainerOperator._

trait DefaultRule extends Operation.Rule with Slf4jLogging.Default {

  override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {
    logDebug(s"getting operator for ${operation.context.noSchema.scalaType.fullName}")
    operation.context.noSchema match {

      case _: Primitive[V] => new PrimitiveOperator(operation)

      case shapeless: ShapelessProduct[V, _] =>
        new ShapelessProductMapper(operation, shapeless)

      case shapeless: ShapelessCoproduct[V, _] =>
        new ShapelessCoproductTupler[V](operation, shapeless)

      case option: OptionContainer[_] =>
        new OptionOperator(
          option.element,
          operation.asInstanceOf[Operation[Option[option.Elem]]]
        )

      case map: MapContainer[_] =>
        new MapOperator(
          map.element,
          operation.asInstanceOf[Operation[Map[String, map.Elem]]]
        )

      case map: MapContainer2[_] =>
        new MapOperator2(
          map.element,
          operation.asInstanceOf[Operation[scala.collection.Map[String, map.Elem]]]
        )

      case seq: SeqContainer[_] =>
        new SeqOperator(
          seq.element,
          operation.asInstanceOf[Operation[Seq[seq.Elem]]]
        )

      case iterable: IterableContainer[_] =>
        new IterableOperator(
          iterable.element,
          operation.asInstanceOf[Operation[Iterable[iterable.Elem]]]
        )
    }
  }.asInstanceOf[Operation.Operator[V]]

}

object DefaultRule extends DefaultRule
