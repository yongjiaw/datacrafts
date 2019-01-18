package org.datacrafts.app.dwfpp

case class FastPass(
  attraction: AttractionWithParkLand,
  time: HourAndMinute,
  party: Set[String]
) {

  override def toString: String = s"{attraction=${attraction}, " +
    s"time=${time}, party=${party}}"

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
