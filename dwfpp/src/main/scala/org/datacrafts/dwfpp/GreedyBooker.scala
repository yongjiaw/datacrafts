package org.datacrafts.dwfpp
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.FatalError
import scala.util.{Failure, Success, Try}

import org.openqa.selenium.{WebDriver, WebElement}
import org.openqa.selenium.chrome.ChromeDriver

import org.datacrafts.dwfpp.Config.Attraction

class GreedyBooker(config: Config) extends FastPassBooker {

  def start(): Unit = {

    {
      startFreshDriver()
      login()
      driver.get("https://disneyworld.disney.go.com/fastpass-plus/select-party/")
      blockUntil("div[id=selectPartyPage]")
      selectParty()
      selectDatePark()
      while (true) {
        selectExperience()
      }
    }.retry(
      callName = Some("outerLoop"),
      maxRetry = None) // retry forever until fatal error

    // closing the driver
    // _currentDriver.foreach(_.close())
  }

  protected def loginAndSelectPartyDatePark(): Unit = {
    login()
    selectParty()
    selectDatePark()
  }

  private var _currentDriver: Option[WebDriver] = None
  override def driver: WebDriver = {
    // System.setProperty("webdriver.chrome.driver", config.chromeDriverLocation)
    _currentDriver.getOrElse(throw new Exception(s"must call startFreshDriver() first"))
  }

  def startFreshDriver(): Unit = {
    _currentDriver.foreach(_.close())
    _currentDriver = Some(
      new ChromeDriver().retry(callName = Some("start driver"), maxRetry = Some(3)) match {
        case Success(driver) => driver
        case Failure(f) => throw new FatalError(s"failed to start diver")
      }
    )
  }

  override def selectionPageSignature: String = "div[id=selectExperiencePage]"

  override def matchAttraction(attractionName: String): AttractionWithParkLand = Try {
    val matched =
    for (
      park <- config.parks;
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

  override def attractionHasValue(name: String): Boolean = {
    matchAttraction(name).attraction.value > 0
  }

  override def parseTime(value: String): HourAndMinute = {
    HourAndMinute.fromString(
      value,
      """^(?i)\s*([0-9]+)\:([0-9]+)\s*([A|P]M).*$""".r)
  }

  override def userName: String = config.login.userName

  override def password: String = config.login.password

  override def partyNames: Seq[String] = config.parties

  override def monthSelection: String = config.date.month

  override def daySelection: Int = config.date.day

  override def parkSelection: String = config.selectedPark.name

  case class TimeWindow(start: HourAndMinute, end: HourAndMinute) {
    def contains(time: HourAndMinute): Boolean = {
      start < time && time < end
    }
  }

  private val _currentSelections = collection.mutable.Set.empty[AttractionSelection]

  override def getState(): Any = {
    Map(
      "currentSelections" -> _currentSelections
    )
  }
  override def evaluateAction(
    selection: ExperienceItem,
    conflicts: Seq[WebElement]
  ): Boolean = {
    val conflictingSelections =
    conflicts.map(AttractionSelection.fromWebElement)
    logInfo(s"conflicting selections: ${conflictingSelections}")
    _currentSelections ++= conflictingSelections

    if (conflictingSelections.isEmpty) {
      true
    }
    else {
      val maxConflictingSelection = conflictingSelections.maxBy(_.score)
      val currentSelection = AttractionSelection.fromExperience(selection)
      val shouldConfirm = currentSelection.score > conflictingSelections.map(_.score).max
      logInfo(s"shouldConfirm=${shouldConfirm}: currentSelection=${currentSelection}, " +
        s"maxConflict=${maxConflictingSelection}")
      shouldConfirm
    }
  }

  override def updateSelection(
    selection: ExperienceItem,
    toBeCancelled: Seq[WebElement],
    confirmed: Boolean
  ): Unit = {
    if (confirmed) {
      _currentSelections += AttractionSelection.fromExperience(selection)
      _currentSelections --= toBeCancelled.map(AttractionSelection.fromWebElement)
    }
    else {
      _currentSelections ++= toBeCancelled.map(AttractionSelection.fromWebElement)
    }
  }

  override def addSelection(attractionSelection: AttractionSelection): Unit = {
    _currentSelections += attractionSelection
  }

  override def getScore(
    name: String,
    time: HourAndMinute
  ): Double = {
    logInfo(s"getting score for ${name}, ${time}")
    val item = AttractionSelection.fromNameAndTime(name, time)
    logInfo(s"item=${item}")
    val conflictingSelections =
      for (
        selection <- _currentSelections
        if {
          selection.attraction == item.attraction ||
          selection.overlaps(item) || selection.conflictsByGroup(item)
        }
      ) yield {
        selection
      }

    logInfo(s"conflicting with existing selections: ${conflictingSelections}")
    logInfo(s"modifying item: ${modifiedItem}")

    val score =
    if (conflictingSelections.nonEmpty) {
      item.score - conflictingSelections.map(_.score).max
    } else {
      // at modify mode, there won't be other conflicting items available to choose from
      item.score - modifiedItem.map(_.score).getOrElse(0d)
    }

    logInfo(s"finale score=${score}: $name, ${time}")

    score
  }
}
