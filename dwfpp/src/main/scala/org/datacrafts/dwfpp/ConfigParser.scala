package org.datacrafts.dwfpp

import java.io.File

import scala.util.matching.Regex

import com.typesafe.config.ConfigFactory

import org.datacrafts.noschema.{NoSchema, NoSchemaDsl}

class ConfigParser[T: NoSchema] extends NoSchemaDsl {

  def parseConfig(fileName: String): T = {
    val configFile = new File(fileName)
    if (configFile.exists()) {
      val config = ConfigFactory.parseFile(configFile).resolve()
      import scala.collection.JavaConverters._
      schemaByShapeless[T].operation().marshal(config.root().unwrapped().asScala)
    } else {
      throw new Exception(s"${configFile} does not exist")
    }

  }
}
