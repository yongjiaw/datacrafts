package org.datacrafts.noschema.rule

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema._
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.operator.{AnyOperator, CoproductTupler, PrimitiveOperator, ProductMapper}
import org.datacrafts.noschema.operator.ContainerOperator._

trait DefaultRule extends Operation.Rule {

  Self: Slf4jLogging =>

  override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {
    logDebug(s"getting operator for ${operation.context.noSchema.scalaType.fullName}")
    operation.context.noSchema match {

      case AnyType => new AnyOperator(operation.asInstanceOf[Operation[Any]])

      case _: Primitive[_] => new PrimitiveOperator(operation)

      case product: NoSchemaProduct[_] =>
        new ProductMapper(operation, product)

      case coproduct: NoSchemaCoproduct[V] =>
        new CoproductTupler(operation, coproduct)

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

      case set: SetContainer[_] =>
        new SetOperator(
          set.element,
          operation.asInstanceOf[Operation[Set[set.Elem]]]
        )

      case iterable: IterableContainer[_] =>
        new IterableOperator(
          iterable.element,
          operation.asInstanceOf[Operation[Iterable[iterable.Elem]]]
        )

      case other =>
        throw new Exception(s"no default operator defined for ${other}")
    }
  }.asInstanceOf[Operation.Operator[V]]

}

object DefaultRule extends DefaultRule with Slf4jLogging.Default
