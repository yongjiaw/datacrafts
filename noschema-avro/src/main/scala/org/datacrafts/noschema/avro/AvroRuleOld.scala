package org.datacrafts.noschema.avro

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Container, NoSchema, Operation, Primitive, ShapelessCoproduct, ShapelessProduct}
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.operator.ContainerOperator.{IterableOperator, MapOperator, MapOperator2, SeqOperator}
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.avro.AvroSchema._
import org.datacrafts.noschema.operator.PrimitiveOperator
import org.datacrafts.noschema.rule.DefaultRule

trait AvroRuleOld extends DefaultRule {
  Self: Slf4jLogging =>
  def getSchema(operation: Operation[_]): AvroSchema = {
    operation.context.noSchema match {

      case primitive: Primitive[_] => new PrimitiveSchema(primitive)

      case shapeless: ShapelessProduct[_, _] =>
        shapeless.scalaType.fullName match {
          case NameSpacePattern(namespace, name) =>
            new ProductRecordSchema(
              name = name,
              namespace = namespace,
              dependencySchema = shapeless.dependencies.map {
                dep => dep.symbol.name -> this.getSchema(operation.dependencyOperation(dep))
              }.toMap
            )
          case _ =>
            throw new Exception(
              s"${shapeless.scalaType.fullName} cannot extract namespace.name")
        }

      case shapeless: ShapelessCoproduct[_, _] if isUnion(shapeless) =>
        new ShapelessCoproductUnionSchema(shapeless, operation, this)

      case shapeless: ShapelessCoproduct[_, _] if isEnum(shapeless) =>
        new ShapelessCoproductEnumSchema(shapeless, operation, this)

      case option: Container[_, _] if option.category == NoSchema.Category.Option =>
        new OptionSchema(this.getSchema(operation.dependencyOperation(option.element)))

      case map: Container[_, _] if map.category == NoSchema.Category.Map =>
        new MapSchema(this.getSchema(operation.dependencyOperation(map.element)))

      case seq: Container[_, _] if seq.category == NoSchema.Category.Seq =>
        new ArraySchema(this.getSchema(operation.dependencyOperation(seq.element)))

    }
  }

  // from shapeless' perspective, there's really no well defined way to tell enum from union
  // scrooge makes them extend different traits, ThriftUnion, ThriftEnum
  // generally, enum's subclasses are all modules(case objects),
  // except that scrooge add one unknown field
  // union's subclasses are all classes
  def isUnion(shapeless: ShapelessCoproduct[_, _]): Boolean = {
    shapeless.scalaType.typeTag.tpe.typeSymbol.asClass
      .knownDirectSubclasses.forall(!_.isModuleClass)
  }

  def isEnum(shapeless: ShapelessCoproduct[_, _]): Boolean = {
    shapeless.scalaType.typeTag.tpe.typeSymbol.asClass
      .knownDirectSubclasses.forall(_.isModuleClass)
  }

  // for unmarshal, need to produce avro accepted data structures
  override def getOperator[V](operation: Operation[V]): Operator[V] = {

    import scala.collection.JavaConverters._
    import scala.reflect.runtime.universe.typeOf
    operation.context.noSchema match {

      case bytes: Primitive[V] if bytes.scalaType.typeTag.tpe <:< typeOf[Array[Byte]] =>
        new PrimitiveOperator[Array[Byte]](
          operation.asInstanceOf[Operation[Array[Byte]]]
        ) {
          override def unmarshalNoneNull(input: Array[Byte]): Any = {
            java.nio.ByteBuffer.wrap(input)
          }
        }.asInstanceOf[Operator[V]]

      case map: MapContainer[_] =>
        new MapOperator(
          map.element,
          operation.asInstanceOf[Operation[Map[String, map.Elem]]]
        ) {
          protected override def unmarshalNoneNull(input: Map[String, Elem]): Any =
            input.map {
              case (k, v) => k -> elementOperation.operator.unmarshal(v)
            }.asJava
        }.asInstanceOf[Operator[V]]

      case map: MapContainer2[_] =>
        new MapOperator2(
          map.element,
          operation.asInstanceOf[Operation[scala.collection.Map[String, map.Elem]]]
        ) {
          protected override def unmarshalNoneNull(
            input: scala.collection.Map[String, Elem]): Any =
            input.map {
              case (k, v) => k -> elementOperation.operator.unmarshal(v)
            }.asJava
        }.asInstanceOf[Operator[V]]

      case seq: SeqContainer[_] =>
        new SeqOperator(
          seq.element,
          operation.asInstanceOf[Operation[Seq[seq.Elem]]]
        ) {
          protected override def unmarshalNoneNull(input: Seq[Elem]): Any =
            input.map(elementOperation.operator.unmarshal).asJava
        }.asInstanceOf[Operator[V]]

      case iterable: IterableContainer[_] =>
        new IterableOperator(
          iterable.element,
          operation.asInstanceOf[Operation[Iterable[iterable.Elem]]]
        ) {
          protected override def unmarshalNoneNull(input: Iterable[Elem]): Any =
            input.map(elementOperation.operator.unmarshal).asJava
        }.asInstanceOf[Operator[V]]

      case _ => super.getOperator(operation)
    }
  }

}

object AvroRuleOld {
  val NameSpacePattern = """(.+)\.(.+)""".r

}