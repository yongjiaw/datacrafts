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

      case option: Container[_, _] if option.category == NoSchema.Category.Option =>
        new OptionOperator(
          option.element,
          operation.asInstanceOf[Operation[Option[option.Elem]]]
        )

      case iterable: Container[_, _]
        if Set(NoSchema.Category.Seq, NoSchema.Category.Set).contains(iterable.category) =>
        new GeneralIterableOperator(
          iterable.element,
          operation.asInstanceOf[Operation[Iterable[iterable.Elem]]]
        )

      case map: Container[_, _] if map.category == NoSchema.Category.Map =>
        new GeneralMapOperator(
          map.element,
          operation.asInstanceOf[Operation[Iterable[(String, map.Elem)]]]
        )

      case other =>
        throw new Exception(s"no default operator defined for ${other}")
    }
  }.asInstanceOf[Operation.Operator[V]]

}

object DefaultRule extends DefaultRule with Slf4jLogging.Default
