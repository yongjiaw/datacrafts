package org.datacrafts.util

import scala.reflect.internal.FatalError
import scala.util.{Failure, Success, Try}

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.util.Retriable.Retry

trait Retriable {

  implicit class CallConverter[T](call: => T) {
    def withRetry(
      maxRetry: Int = 10,
      maxWait: Long = Long.MaxValue,
      callName: String = "unnamed"
    ): T = {
      new Retry(call, maxRetry, maxWait, callName).retry() match {
        case Success(result) => result
        case Failure(f) => throw f
      }
    }
  }

}

object Retriable {
  class Retry[T](
    call: => T,
    maxRetry: Int,
    maxWait: Long,
    callName: String
  ) extends Slf4jLogging.Default {

    val startTime: Long = System.currentTimeMillis()

    // can override this method to achieve arbitrary behavior
    protected def shouldRetryAndWait(tried: Int, f: Throwable): Option[Long] = {

      val waited = System.currentTimeMillis() - startTime
      val waitTime =
        f match {
          case FatalError(f) => None
          case _ => if (tried < maxRetry && waited < maxWait) Some(1000L) else None
        }
      if (waitTime.isEmpty) {
        logError(s"call [$callName] " +
          s"terminated with ${tried} previous retries, maxRetry=${maxRetry}, " +
          s"waited=$waited, maxWait=${maxWait}: ${f.getMessage}")
      }
      else {
        logDebug(s"call [$callName] " +
          s"will retry with ${tried} previous retries, maxRetry=${maxRetry}, " +
          s"waited=$waited, maxWait=${maxWait}, nextWait=${waitTime}: ${f.getMessage}")
      }

      waitTime

    }

    def retry(): Try[T] = {
      retryInternal(n = 0)
    }

    private def retryInternal(n: Int): Try[T] = {

      Try(call) match {
        case Failure(f) =>
          shouldRetryAndWait(n, f) match {
            case Some(waitMs) =>
              Thread.sleep(waitMs)
              retryInternal(n + 1)
            case None =>
              Failure(f)
          }
        case success => success
      }
    }
  }
}
