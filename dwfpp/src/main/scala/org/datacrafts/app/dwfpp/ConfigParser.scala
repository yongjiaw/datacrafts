package org.datacrafts.app.dwfpp

import java.io.File

import com.typesafe.config.ConfigFactory

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{NoSchema, NoSchemaDsl, Operation, Primitive}
import org.datacrafts.noschema.operator.PrimitiveOperator
import org.datacrafts.noschema.rule.DefaultRule

class ConfigParser[T: NoSchema] extends NoSchemaDsl {

  def parseConfig(fileName: String): T = {
    val configFile = new File(fileName)
    if (configFile.exists()) {
      val config = ConfigFactory.parseFile(configFile).resolve()
      import scala.collection.JavaConverters._
      schemaByShapeless[T].operation(
        new DefaultRule with Slf4jLogging.Default {
          override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {

            operation.context.noSchema match {

              case o: Primitive[_] if o.refinedType == Primitive.Type.Double =>
                new PrimitiveOperator(operation) {
                  override def marshalNoneNull(input: Any): V = {
                    input.toString.toDouble.asInstanceOf[V]
                  }
                }
              case _ => super.getOperator(operation)
            }
          }
        }
      ).marshal(config.root().unwrapped().asScala)
    } else {
      throw new Exception(s"${configFile} does not exist")
    }

  }
}
