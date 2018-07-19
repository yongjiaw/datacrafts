package org.datacrafts.noschema.avro

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{NoSchemaDsl, Operation, Primitive, ShapelessCoproduct, ShapelessProduct}
import org.datacrafts.noschema.operator.PrimitiveOperator
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.operator.ContainerOperator.{IterableOperator, MapOperator, MapOperator2, SeqOperator}
import org.datacrafts.noschema.rule.DefaultRule

object DefaultAvroRule extends DefaultAvroRule with Slf4jLogging.Default {

  val NameSpacePattern = """(.+)\.(.+)""".r

  val javaMapper = new ObjectMapper()
  val scalaMapper = new ObjectMapper() with ScalaObjectMapper
  scalaMapper.registerModule(DefaultScalaModule)
  scalaMapper.setSerializationInclusion(Include.NON_ABSENT)

  def toJson(value: Any): String = {
    scalaMapper.writeValueAsString(value)
  }
}

trait DefaultAvroRule extends DefaultRule with NoSchemaDsl {

  Self: Slf4jLogging =>

  import DefaultAvroRule._

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

      case shapeless: ShapelessProduct[V, _] =>
        new ShapelessProductAvroOperator[V](shapeless, operation, this)

      case shapeless: ShapelessCoproduct[V, _] =>
        new ShapelessCoproductAvroOperator[V](shapeless, operation, this)

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

  def getAvroSchema[V](operation: Operation[V]): Schema = {
    import scala.reflect.runtime.universe.typeOf
    import scala.collection.JavaConverters._
    val schema =
      operation.context.noSchema match {
        case primitive: Primitive[_] =>
          primitive.scalaType.typeTag.tpe match {
            case t if t <:< typeOf[Short] => Schema.create(Schema.Type.INT)
            case t if t <:< typeOf[Int] => Schema.create(Schema.Type.INT)
            case t if t <:< typeOf[Long] => Schema.create(Schema.Type.LONG)
            case t if t <:< typeOf[Double] => Schema.create(Schema.Type.DOUBLE)
            case t if t <:< typeOf[Float] => Schema.create(Schema.Type.FLOAT)
            case t if t <:< typeOf[Boolean] => Schema.create(Schema.Type.BOOLEAN)
            case t if t <:< typeOf[String] => Schema.create(Schema.Type.STRING)
            case t if t <:< typeOf[Array[Byte]] => Schema.create(Schema.Type.BYTES)
          }
        case shapeless: ShapelessProduct[_, _] =>
          shapeless.scalaType.fullName match {
            case NameSpacePattern(namespace, name) =>
              Schema.createRecord(
                name,
                toJson(
                  Map("record" -> shapeless.scalaType.uniqueKey)
                ),
                namespace,
                false,
                shapeless.dependencies.map(
                  dep => {
                    val depOp = new AvroOperation(operation.dependencyOperation(dep), this)
                    new Field(
                      dep.symbol.name,
                      depOp.avroSchema,
                      toJson(
                        Map(dep.symbol.name -> dep.noSchema.scalaType.uniqueKey)
                      ),
                      // no default value needed for avro schema,
                      // since there won't be missing values
                      // when generating avro record from scala class.
                      // default in avro schema plays the same role as the default in operator,
                      // which is needed when marshaling from avro input to scala class
                      null.asInstanceOf[Any]
                    )
                  }
                ).asJava
              )
            case _ =>
              throw new Exception(
                s"${shapeless.scalaType.fullName} cannot extract namespace.name")
          }

        case shapeless: ShapelessCoproduct[_, _] =>
          if (isUnion(shapeless)
          ) { // enum sealed trait contains case objects, except for the default unknown value
            Schema.createUnion(
              shapeless.dependencies.map {
                dep =>
                  val depOp = new AvroOperation(operation.dependencyOperation(dep), this)
                  depOp.avroSchema
              }.asJava
            )
          } else if (isEnum(shapeless)) {
            shapeless.scalaType.fullName match {
              case NameSpacePattern(_, name) =>
                Schema.createEnum(
                  name,
                  toJson(
                    Map("enumClass" -> shapeless.scalaType.uniqueKey)
                  ),
                  shapeless.scalaType.fullName,
                  shapeless.dependencies.map {
                    dep => dep.noSchema.scalaType.shortName
                  }.asJava
                )
              case _ =>
                throw new Exception(
                  s"${shapeless.scalaType.fullName} cannot extract namespace.name")
            }
          }
          else {
            throw new Exception(s"neither union nor enum\n${shapeless.format()}")
          }

        case option: OptionContainer[_] =>
          val elementSchema =
            new AvroOperation(operation.dependencyOperation(option.element), this).avroSchema
          // create union type to allow null value if scala type is Option
          if (elementSchema.getType == Schema.Type.UNION) {
            elementSchema
          } else {
            Schema.createUnion(
              Schema.create(Schema.Type.NULL),
              elementSchema
            )
          }

        case map: MapContainer[_] =>
          val elementSchema =
            new AvroOperation(operation.dependencyOperation(map.element), this).avroSchema
          Schema.createMap(elementSchema)

        case map: MapContainer2[_] =>
          val elementSchema =
            new AvroOperation(operation.dependencyOperation(map.element), this).avroSchema
          Schema.createMap(elementSchema)

        case seq: SeqContainer[_] =>
          val elementSchema =
            new AvroOperation(operation.dependencyOperation(seq.element), this).avroSchema
          Schema.createArray(elementSchema)

        case iterable: IterableContainer[_] =>
          val elementSchema =
            new AvroOperation(operation.dependencyOperation(iterable.element), this).avroSchema
          Schema.createArray(elementSchema)

        case other => throw new Exception(s"Missing avro schema rule for:\n${other.format()}")
      }
    schema
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

}
