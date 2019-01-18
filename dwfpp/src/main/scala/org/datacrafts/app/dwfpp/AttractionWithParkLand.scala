package org.datacrafts.app.dwfpp

import scala.reflect.internal.FatalError
import scala.util.{Failure, Success, Try}

import org.datacrafts.app.dwfpp.BookerConfig.Attraction
import org.datacrafts.logging.Slf4jLogging

case class AttractionWithParkLand(
  rawName: String,
  park: BookerConfig.Park,
  land: BookerConfig.Land,
  attraction: Attraction) {
  override def toString: String = s"[park=${park.name} selected=${park.selected} " +
    s"land=${land.name} attraction=${attraction.name}]"

  val groupId: Option[(BookerConfig.Park, String)] = {
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
    parks: Seq[BookerConfig.Park],
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
      AttractionWithParkLand(
        attractionName,
        BookerConfig.UnknownPark,
        BookerConfig.UnknownLand,
        BookerConfig.UnknownAttraction)
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
