package org.datacrafts.dwfpp

import org.openqa.selenium.{WebDriver, WebElement}
import org.openqa.selenium.chrome.ChromeDriver

object ChromeFastPassBooker {
  def main(args: Array[String]): Unit = {
    val configParser = new ConfigParser[FastPassBookerConfig]
    // val config = configParser.parseConfig("dwfpp/application.conf")
    if (args.isEmpty) {
      throw new Exception(s"must provide config as input")
    }

    val config = configParser.parseConfig(args(0))

    val instance = new ChromeFastPassBooker(config)

    // instance.logIn()

    // go to the initial page for manual interactive setup
    instance.driver.get(config.fastPassConfig.initialPageUrl)

    instance.selectParty()

    instance.selectDatePark()

    // automate the selection process
    instance.selectExperience()
  }
}

class ChromeFastPassBooker(
  config: FastPassBookerConfig
) extends FastPassBooker {

  val partyNames = Seq()

  val userName = ""
  val password = ""
  override lazy val driver: WebDriver = {
    System.setProperty("webdriver.chrome.driver", config.chromeDriverLocation)
    new ChromeDriver()
  }

  override def evaluateAction(selection: ExperienceItem,
    toBeCancelled: Seq[WebElement]
  ): Boolean = {
    // add cancel items to already selected attractions
    super.evaluateAction(selection, toBeCancelled)
  }

  override def updateSelection(selection: ExperienceItem,
    toBeCancelled: Seq[WebElement], confirmed: Boolean
  ): Unit = {
    // remove cancelled items, add new selection
    super.updateSelection(selection, toBeCancelled, confirmed)
  }

  override def selectionPageSignature: String = "div[id=selectExperiencePage]"

  override def attractionHasValue(name: String): Boolean =
    config.fastPassConfig.scoringRules.filter(_.value > 0).exists(
      detail => name.matches(detail.namePattern))

  override def getScore(
    name: String,
    time: HourAndMinute
  ): Double = {
    config.fastPassConfig.scoringRules.map {
      case ScoringRule(namePattern, timeSlot, value) =>
        if (name.matches(namePattern) && timeSlot.contains(time)) value else 0
    }.sum
    }

  override def parseTime(value: String): HourAndMinute =
    HourAndMinute.fromString(value, config.fastPassConfig.timeRegex)

  override def monthSelection: String = "January"

  override def daySelection: Int = 14

  // override def parkSelection: String = "Animal Kingdom"
  override def parkSelection: String = "Epcot"

  override def addSelection(attractionSelection: AttractionSelection): Unit = {}

  override def matchAttraction(attractionName: String): AttractionWithParkLand = null
}
