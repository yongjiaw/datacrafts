package org.datacrafts.logging

import org.scalatest.FlatSpec

class LazyLoggingTest extends FlatSpec with Slf4jLogging.Default {
  "logging message evaluation" should "be lazy" in {
    var value1 = 1
    var value2 = 1

    logInfo(s"info ${value1 = 2; value1}")
    logWarning(s"warning ${value2 = 2; value2}", new Exception("exception log as expected"))
    logError(s"error")

    // log level is above info, the re-assignment should not happen
    assert(!dataCraftsLogger.isInfoEnabled && value1 == 1)
    // warn should be evaluated
    assert(dataCraftsLogger.isWarnEnabled && value2 == 2)
  }
}
