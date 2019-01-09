package org.datacrafts.dwfpp

import scala.reflect.internal.FatalError
import scala.util.{Failure, Success, Try}

import org.datacrafts.app.dwfpp.FastPass
import org.datacrafts.dwfpp.Config.Attraction
import org.datacrafts.logging.Slf4jLogging

case class AttractionSelection(
  attraction: AttractionWithParkLand,
  time: HourAndMinute
) {
  override def toString: String = s"{attraction=${attraction}, time=${time}, score=${score}}"
  def score: Double = {
    if (attraction == UnknownAttraction) {
      0
    } else {
      attraction.attraction.value
    }
  }

  def conflictsByGroup(other: AttractionSelection): Boolean = {
    attraction.park == other.attraction.park &&
      attraction.attraction.group.nonEmpty &&
      attraction.attraction.group == other.attraction.attraction.group &&
      attraction.park.groupLimitMap.get(
        attraction.attraction.group.get
      ).map(_ == 1).getOrElse(false)

  }
  def overlaps(other: AttractionSelection): Boolean = {
    !notOverlap(other)
  }

  def notOverlap(other: AttractionSelection): Boolean = {
    val end = time.copy(hour = time.hour + 1)
    val otherEnd = other.time.copy(hour = other.time.hour + 1)
    end <= other.time || otherEnd <= time
  }
}

case class AttractionWithParkLand(
  rawName: String,
  park: Config.Park,
  land: Config.Land,
  attraction: Attraction) {
  override def toString: String = s"[park=${park.name} selected=${park.selected} " +
    s"land=${land.name} attraction=${attraction.name}]"

  val groupId: Option[(Config.Park, String)] = {
    attraction.group.map {
      groupName => (park, groupName)
    }
  }

  def conflictsByGroup(other: AttractionWithParkLand): Boolean = {
    park == other.park &&
      attraction.group.nonEmpty &&
      attraction.group == other.attraction.group &&
      park.groupLimitMap.get(
        attraction.group.get
      ).map(_ == 1).getOrElse(false)

  }

}

object AttractionWithParkLand extends Slf4jLogging.Default {
  def matchAttraction(
    parks: Seq[Config.Park],
    attractionName: String
  ): AttractionWithParkLand = Try {
    val matched =
      for (
        park <- parks;
        land <- Option(park.lands).getOrElse(Seq.empty);
        attraction <- Option(land.attractions).getOrElse(Seq.empty)
        if attractionName.toUpperCase.contains(attraction.name.toUpperCase)
      ) yield {
        AttractionWithParkLand(attractionName, park, land, attraction)
      }
    if (matched.isEmpty) {
      logWarning(s"${attractionName} does not match to known attractions")
      UnknownAttraction(attractionName)
    }
    else if (matched.size == 1) {
      matched.head
    } else {
      throw new FatalError(s"${attractionName} must match with exactly one attraction: ${matched}")
    }
  } match {
    case Success(result) => result
    case Failure(f) => throw FatalError(s"failed to match attraction ${f.getMessage}")
  }
}

object UnknownAttraction {
  def apply(name: String): AttractionWithParkLand =
    AttractionWithParkLand(
      name,
      Config.UnknownPark, Config.UnknownLand,
      Config.UnknownAttraction)
}

