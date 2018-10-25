package org.datacrafts.noschema.avro

import scala.collection.JavaConverters._

import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Container, NoSchema, NoSchemaCoproduct, NoSchemaDsl, NoSchemaProduct, Operation, Primitive}
import org.datacrafts.noschema.Container._
import org.datacrafts.noschema.Context.{CoproductElement, LocalContext}
import org.datacrafts.noschema.Operation.Operator
import org.datacrafts.noschema.avro.AvroContainerOperator._
import org.datacrafts.noschema.avro.AvroRule.SchemaWrapper
import org.datacrafts.noschema.operator.PrimitiveOperator
import org.datacrafts.noschema.rule.DefaultRule

object AvroRule {
  val NameSpacePattern = """(.+)\.(.+)""".r

  object Default extends AvroRule with Slf4jLogging.Default

  case class SchemaWrapper(name: String, nameSpace: String, wrapperField: String)
}

trait AvroRule extends DefaultRule with NoSchemaDsl {
  Self: Slf4jLogging =>

  def getSchemaDoc(operation: AvroOperation[_]): String = {
    null // scalastyle:ignore
  }

  def getFieldDoc(operation: AvroOperation[_]): String = {
    null // scalastyle:ignore
  }

  // from shapeless' perspective, there's really no well defined way to tell enum from union
  // scrooge makes them extend different traits, ThriftUnion, ThriftEnum
  // generally, enum's subclasses are all modules(case objects),
  // except that scrooge add one unknown field
  // union's subclasses are all classes
  def isUnion(coproduct: NoSchemaCoproduct[_]): Boolean = {
    coproduct.scalaType.typeTag.tpe.typeSymbol.asClass
      .knownDirectSubclasses.forall(!_.isModuleClass)
  }

  def isEnum(coproduct: NoSchemaCoproduct[_]): Boolean = {
    coproduct.scalaType.typeTag.tpe.typeSymbol.asClass
      .knownDirectSubclasses.forall(_.isModuleClass)
  }

  def getEnumValue(context: LocalContext[_]): String = {
    context.noSchema.scalaType.shortName
  }

  def getSchemaWrapper(operation: AvroOperation[_]): Option[SchemaWrapper] = {

    operation.parentOp match {
      // avro does not support nested union
      // if the member of a union is also a union, must wrap it
      case Some(parentOp) if parentOp.isUnion && operation.isUnion =>
        operation.context.localContext match {
          case coproductElem: CoproductElement[_] =>
            Some(
              SchemaWrapper(
                name = coproductElem.symbol.name,
                nameSpace = parentOp.context.noSchema.scalaType.fullName,
                wrapperField = getWrapFieldName(operation)
              )
            )
          case other =>
            throw new Exception(
              s"does not have rule to wrap context ${operation.context}, " +
                s"need to extend the wrapper rule")
        }

      case _ => None
    }
  }

  def getWrapFieldName(wrappedOperation: AvroOperation[_]): String = {
    wrappedOperation.context.noSchema.scalaType.shortName
  }

  def getSchema(operation: AvroOperation[_]): Schema = {
    operation.context.noSchema match {

      // primitive
      case primitive: Primitive[_] =>
        primitive.refinedType match {
          case Primitive.Type.Short => Schema.create(Schema.Type.INT)
          case Primitive.Type.Int => Schema.create(Schema.Type.INT)
          case Primitive.Type.Long => Schema.create(Schema.Type.LONG)
          case Primitive.Type.Double => Schema.create(Schema.Type.DOUBLE)
          case Primitive.Type.Float => Schema.create(Schema.Type.FLOAT)
          case Primitive.Type.Boolean => Schema.create(Schema.Type.BOOLEAN)
          case Primitive.Type.String => Schema.create(Schema.Type.STRING)
          case Primitive.Type.Bytes => Schema.create(Schema.Type.BYTES)
          case _ => throw new Exception(s"no primitive avro schema for ${primitive}")
        }

      // option, add null to Union if needed
      case option: Container[_, _] if option.category == NoSchema.Category.Option =>
        val elementOp = operation.dependencyOperation(option.element)
        if (elementOp.isNullable) {
          elementOp.avroSchema
        } else if ( // union but not nullable
          elementOp.avroSchema.getType == Schema.Type.UNION
        ) {
          // add NULL type to the union
          Schema.createUnion(
            (elementOp.avroSchema.getTypes.asScala :+ Schema.create(Schema.Type.NULL)).asJava
          )
        } else { // not union can be wrapped
          Schema.createUnion(
            elementOp.avroSchema,
            Schema.create(Schema.Type.NULL)
          )
        }

      // map
      case map: Container[_, _] if map.category == NoSchema.Category.Map =>
        Schema.createMap(operation.dependencyOperation(map.element).avroSchema)

      // array
      case seq: Container[_, _] if seq.category == NoSchema.Category.Seq =>
        Schema.createArray(operation.dependencyOperation(seq.element).avroSchema)

      // record
      case product: NoSchemaProduct[_] =>
        product.scalaType.fullName match {
          case AvroRule.NameSpacePattern(namespace, name) =>
            Schema.createRecord(
              name,
              getSchemaDoc(operation),
              namespace,
              false,
              product.fields.map {
                case dep =>
                  val depOp = operation.dependencyOperation(dep)
                  new Field(
                    dep.symbol.name,
                    depOp.avroSchema,
                    getFieldDoc(depOp),
                    // no default value needed for avro schema,
                    // since there won't be missing values
                    // when generating avro record from scala class.
                    // default in avro schema plays the same role as the default in operator,
                    // which is needed when marshaling from avro input to scala class
                    null.asInstanceOf[Any] // scalastyle:ignore
                  )
              }.toSeq.sortBy(_.name()).asJava
            )

          case _ =>
            throw new Exception(
              s"${product.scalaType.fullName} cannot extract namespace.name")
        }

      // union
      case coproduct: NoSchemaCoproduct[_] if operation.isUnion =>
        Schema.createUnion(
          coproduct.members.map {
            dep =>
              operation.dependencyOperation(dep).avroSchema
          }.sortBy(_.getName).asJava
        )

      // enum
      case coproduct: NoSchemaCoproduct[_] if operation.isEnum =>
        coproduct.scalaType.fullName match {
          case AvroRule.NameSpacePattern(namespace, name) =>
            Schema.createEnum(
              name,
              getSchemaDoc(operation),
              namespace,
              coproduct.members.map {
                dep => getEnumValue(dep)
              }.asJava
            )

          case _ =>
            throw new Exception(
              s"${coproduct.scalaType.fullName} cannot extract namespace.name")
        }

      case coproduct: NoSchemaCoproduct[_] =>
        throw new Exception(
          s"Coproduct is neither union nor enum, need to update avroRule to cover the case" +
            s"\n${coproduct.format()}")

      case other => throw new Exception(s"Missing avro schema rule for:\n${other.format()}")
    }
  }

  // for unmarshal, need to produce avro accepted data structures
  def getAvroOperator[V](operation: AvroOperation[V]): Operator[V] = {

    operation.context.noSchema match {

      case bytes: Primitive[_] if bytes.refinedType == Primitive.Type.Bytes =>
        new PrimitiveOperator[Array[Byte]](
          operation.asInstanceOf[Operation[Array[Byte]]]
        ) {
          override def toString: String = s"AvroBytesOperator"
          override def marshalNoneNull(input: Any): Array[Byte] = {
            input match {
              case byteBuffer: java.nio.ByteBuffer => byteBuffer.array()
              case _ => super.marshalNoneNull(input)
            }
          }
          override def unmarshalNoneNull(input: Array[Byte]): Any = {
            java.nio.ByteBuffer.wrap(input)
          }
        }.asInstanceOf[Operator[V]]

      case short: Primitive[_] if short.refinedType == Primitive.Type.Short =>
        new PrimitiveOperator[Short](
          operation.asInstanceOf[Operation[Short]]
        ) {
          override def toString: String = s"AvroShortOperator"
          override def marshalNoneNull(input: Any): Short = {
            input.toString.toShort
          }
          // avro does not support short value, it's mapped to INT
          override def unmarshalNoneNull(input: Short): Any = {
            input.intValue()
          }
        }.asInstanceOf[Operator[V]]

      case product: NoSchemaProduct[_] =>
        new ProductAvroOperator(product, operation, this)

      case coproduct: NoSchemaCoproduct[_] =>
        new CoproductAvroOperator(coproduct, operation, this)

      case map: MapContainer[_] =>
        new AvroMapOperator(
          map.element,
          operation.asInstanceOf[AvroOperation[Map[String, map.Elem]]]
        )

      case map: MapContainer2[_] =>
        new AvroMapOperator2(
          map.element,
          operation.asInstanceOf[AvroOperation[scala.collection.Map[String, map.Elem]]]
        )

      case seq: SeqContainer[_] =>
        new AvroSeqOperator(
          seq.element,
          operation.asInstanceOf[AvroOperation[Seq[seq.Elem]]]
        )

      case iterable: IterableContainer[_] =>
        new AvroIterableOperator(
          iterable.element,
          operation.asInstanceOf[AvroOperation[Iterable[iterable.Elem]]]
        )

      case option: OptionContainer[_] =>
        new AvroOptionOperator(
          option.element,
          operation.asInstanceOf[AvroOperation[Option[option.Elem]]]
        )

      case _ => super.getOperator(operation)
    }
  }.asInstanceOf[Operator[V]]
}
