package org.datacrafts.logging

/**
  * This trait controls dependency injection of the logger implementation
  * Slf4j's logging interface is general enough, and it seems to be the future proof JVM standard.
  * However, its initialization is not transparent and flexible enough, due to its java heritage.
  * Here, the opportunity of reinventing a better wheel is exposed in more general way.
  * We may need multiple logger instances each controlled by its own explicit configuration
  * We also want to invoke logging as method of the mixin class
  */
trait Slf4jLogging {

  // this method has full control of logger implementation injection by implementing class
  def dataCraftsLogger: org.slf4j.Logger

  // the following methods can achieve
  // 1. makes the logging calls more convenient
  // 2. use scalalogging which uses macro to generate the code that checks log level
  // before evaluating the parameters
  // we can expose more logging methods (such as using marker) when the need arises
  private lazy val scalaLogger = com.typesafe.scalalogging.Logger(dataCraftsLogger)
  final def logDebug(msg: => String): Unit = {
    scalaLogger.debug(msg)
  }

  final def logInfo(msg: => String): Unit = {
    scalaLogger.info(msg)
  }

  final def logInfo(msg: => String, t: Throwable): Unit = {
    scalaLogger.info(msg, t)
  }

  final def logWarning(msg: => String): Unit = {
    scalaLogger.warn(msg)
  }

  final def logWarning(msg: => String, t: Throwable): Unit = {
    scalaLogger.warn(msg, t)
  }

  final def logError(msg: => String): Unit = {
    scalaLogger.error(msg)
  }

  final def logError(msg: => String, t: Throwable): Unit = {
    scalaLogger.error(msg, t)
  }

  final def logTrace(msg: => String): Unit = {
    scalaLogger.trace(msg)
  }

  final def logTrace(msg: => String, t: Throwable): Unit = {
    scalaLogger.trace(msg, t)
  }
}

object Slf4jLogging {
  trait Default extends Slf4jLogging {
    // Method to get the logger name for this object
    protected def dataCraftsLogName = {
      // Ignore trailing $'s in the class names for Scala objects
      this.getClass.getName.stripSuffix("$")
    }

    // the compatible logger implementations are injected without parameter
    @transient override lazy val dataCraftsLogger: org.slf4j.Logger =
    org.slf4j.LoggerFactory.getLogger(dataCraftsLogName)
  }
}
