package org.datacrafts.util

import scala.reflect.internal.FatalError

import org.datacrafts.logging.Slf4jLogging

object RetriableExample extends Retriable with Slf4jLogging.Default {
  def main(args: Array[String]): Unit = {
    var tried: Int = 0

    {
      tried += 1
      if (tried < 10) {
        throw new Exception("failed")
      } else {
        throw new FatalError("failed")
      }
      Unit
    }.withRetry()
  }
}
