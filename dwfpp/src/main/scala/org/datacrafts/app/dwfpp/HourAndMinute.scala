package org.datacrafts.app.dwfpp

import scala.reflect.internal.FatalError
import scala.util.matching.Regex

case class HourAndMinute(hour: Int, min: Int) {
  def < (other: HourAndMinute): Boolean = {
    hour < other.hour || hour == other.hour && min < other.min
  }

  def <= (other: HourAndMinute): Boolean = {
    hour < other.hour || hour == other.hour && min <= other.min
  }
  def timeValue: Double = hour + min * 1.0 / 60
}

object HourAndMinute {
  val HourAndMinutePattern = """^(?i)\s*([0-9]+)\:([0-9]+)\s*([A|P]M|).*$""".r
  def fromString(value: String, regex: Regex = HourAndMinutePattern): HourAndMinute = {
    value match {
      case regex(hour, min, part) =>
        HourAndMinute(
          if (part.toUpperCase() == "PM" && hour.toInt < 12) hour.toInt + 12 else hour.toInt,
          min.toInt)
      case _ => throw new FatalError(s"${value} does not match pattern ${HourAndMinutePattern}")
    }
  }
}
