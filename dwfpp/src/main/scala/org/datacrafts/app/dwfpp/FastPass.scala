package org.datacrafts.app.dwfpp

import java.util.{Calendar, TimeZone}

import org.datacrafts.dwfpp.{AttractionSelection, AttractionWithParkLand, HourAndMinute}
import org.datacrafts.dwfpp.Config.UnknownAttraction

case class FastPass(
  attraction: AttractionWithParkLand,
  time: HourAndMinute,
  party: Set[String]
) {

  override def toString: String = s"{attraction=${attraction}, " +
    s"time=${time}, party=${party}, value=${value}}"

  def value: Double = {
    if (attraction == UnknownAttraction) {
      0
    } else {
      attraction.attraction.value
    }
  }

  def conflictWith(other: FastPass): Boolean = {
    party.intersect(other.party).nonEmpty && {
      conflictsByGroup(other) || hasTimeConflict(other)
    }
  }

  def conflictsByGroup(other: FastPass): Boolean = {
    attraction.conflictsByGroup(other.attraction)
  }

  def hasTimeConflict(other: FastPass): Boolean = {
    !noTimeConflict(other)
  }

  def noTimeConflict(other: FastPass): Boolean = {
    val end = time.copy(hour = time.hour + 1)
    val otherEnd = other.time.copy(hour = other.time.hour + 1)
    end <= other.time || otherEnd <= time
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val cal = Calendar.getInstance(
      TimeZone.getTimeZone("EST")
    )
    val month = cal.get(Calendar.MONTH)
    val day = cal.get(Calendar.DAY_OF_MONTH)
    println(s"${month} $day")
  }
}