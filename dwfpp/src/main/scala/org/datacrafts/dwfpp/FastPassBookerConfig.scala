package org.datacrafts.dwfpp

import scala.util.matching.Regex

case class FastPassBookerConfig(
  chromeDriverLocation: String,
  fastPassConfig: FastPassConfig
)

case class FastPassConfig(
  initialPageUrl: String,
  landingPageSignature: String,
  timeParsingRegex: String,
  scoringRules: Seq[ScoringRule]
) {
  val timeRegex = timeParsingRegex.r
}

case class ScoringRule(
  namePattern: String,
  timeSlot: TimeSlot,
  value: Double
) {
  val namePatternRegex = namePattern.r
}

case class TimeSlot(
  start: String,
  end: String
) {
  val startTimeHour = HourAndMinute.fromString(start) // parse from the config file
  val endTimeHour = HourAndMinute.fromString(end)
  if (endTimeHour < startTimeHour) {
    throw new Exception(s"endTime cannot be before startTime: start=${start}, end=${end}")
  }
  def contains(time: HourAndMinute): Boolean = {
    (startTimeHour < time || startTimeHour == time) && (time < endTimeHour || time == endTimeHour)
  }
}

case class HourAndMinute(hour: Int, min: Int) {
  def < (other: HourAndMinute): Boolean = {
    hour < other.hour || hour == other.hour && min < other.min
  }

  def timeValue: Double = hour + min * 1.0 / 60
}

object HourAndMinute {
  val HourAndMinutePattern = """^(?i)\s*([0-9]+)\:([0-9]+)\s*([A|P]M|)\s*$""".r
  def fromString(value: String, regex: Regex = HourAndMinutePattern): HourAndMinute = {
    value match {
      case regex(hour, min, part) =>
        HourAndMinute(
          if (part.toUpperCase() == "PM" && hour.toInt < 12) hour.toInt + 12 else hour.toInt,
          min.toInt)
      case _ => throw new Exception(s"${value} does not match pattern ${HourAndMinutePattern}")
    }
  }
}

object TestLoader extends ConfigParser[FastPassBookerConfig] {
  def main(args: Array[String]): Unit = {
    println(
      parseConfig("./dwfpp/application.conf")
    )
  }
}
