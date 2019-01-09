package org.datacrafts.dwfpp

import java.util.{Calendar, TimeZone}

import org.datacrafts.dwfpp.Config.Login

object Config {

  def parse(fileName: String): Config = {
    new ConfigParser[Config].parseConfig(fileName)
  }

  case class Login (userName: String, password: String)

  case class Date(month: String, day: Int)

  case class Park(
    name: String,
    selected: Option[Boolean],
    lands: Seq[Land],
    groups: Option[Seq[Config.Group]] = None
  ) {

    val groupLimitMap: Map[String, Int] = groups.getOrElse(Seq.empty).map {
      g => g.name -> g.limit
    }.toMap
  }

  object UnknownPark extends Park("unknown", None, Seq.empty)

  case class Land(
    name: String,
    attractions: Seq[Attraction]
  )

  object UnknownLand extends Land("unknown", Seq.empty)

  case class Attraction(
    name: String,
    value: Double,
    hourPreferences: Option[Seq[HourValue]],
    group: Option[String],
    modifyCycles: Option[Int]
  )

  object UnknownAttraction extends Attraction("Unknown", 0, None, None, None)

  case class Group(name: String, limit: Int)

  case class HourValue(hour: Double, value: Double)
}

case class Config(
  login: Login,
  parties: Seq[String],
  date: Config.Date,
  hourPreferences: Option[Seq[Config.HourValue]],
  parks: Seq[Config.Park],
  timeZone: String,
  logRoot: String
) {
  val calender = Calendar.getInstance(
    TimeZone.getTimeZone(Option(timeZone).getOrElse("EST"))
  )
  def selectedPark: Config.Park = {
    val selected = parks.filter(_.selected.getOrElse(false))
    if (selected.size != 1) {
      throw new Exception(s"must select exactly 1 park: selected=${selected.map(_.name)}")
    }
    selected.head
  }
}
