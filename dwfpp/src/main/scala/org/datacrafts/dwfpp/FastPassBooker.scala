package org.datacrafts.dwfpp

import java.util.LongSummaryStatistics

import scala.util.{Failure, Success, Try}

import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}

import org.datacrafts.logging.Slf4jLogging

trait FastPassBooker extends Slf4jLogging.Default {

  def driver: WebDriver

  // def config: FastPassBookerConfig

  def landingPageSignature: String

  def matchName(name: String): Boolean

  def getScore(name: String,
    time: HourAndMinute
  ): Double

  def parseTime(value: String): HourAndMinute

  import scala.collection.JavaConverters._

  protected def pageContent: String = {
    // driver.findElement(By.tagName("html")).getText
    driver.getPageSource
  }

  private var currentContent = ""

  protected def pageLoadTimeout: Long = 10000

  protected def pageLoadPollingInterval: Long = 1000

  protected def waitForPageLoading(): Unit = {
    val startTime = System.currentTimeMillis()
    if (currentContent != pageContent) {
      do {
        logInfo("page still loading")
        currentContent = pageContent
        Thread.sleep(pageLoadPollingInterval)
      } while ( {
        if (currentContent != pageContent &&
          System.currentTimeMillis() - startTime < pageLoadTimeout) {
          true
        }
        else {
          if (currentContent == pageContent) {
            logInfo("page stopped loading")
          } else {
            logInfo(s"page loading timeout after ${pageLoadTimeout}ms")
          }
          false
        }
      }
      )
    }
  }

  implicit class StringConverter(value: String) {

    def cssElementsOf(parent: Option[WebElement] = None): Seq[WebElement] = {
      waitForPageLoading()
      parent match {
        case Some(p) => p.findElements(By.cssSelector(value)).asScala
        case None => driver.findElements(By.cssSelector(value)).asScala
      }
    }

    def cssOptionalElementOf(parent: Option[WebElement] = None): Option[WebElement] = {
      val elements = cssElementsOf(parent)
      if (elements.size > 1) {
        throw new Exception(
          s"css selector [${value}] cannot have more than one match: size=${elements.size}")
      }
      elements.headOption

    }

    def cssSingleElementOf(parent: Option[WebElement] = None): WebElement = {
      cssOptionalElementOf(parent).getOrElse(
        throw new Exception(
          s"css selector [${value}] must have exactly one match")
      )
    }
  }

  protected def blockUntilSelectionPage(): Unit = {
    blockUntil(
      landingPageSignature,
      Some(
        s"required interactive steps before automation (never change font size): go to browser, " +
          s"login, select party, select date, select park, " +
          s"click desirable day part (Morning, Afternoon, Evening), "
      )
    )
  }

  protected def showAllExperiences(): Unit = {
    "a[class^=showMore]".cssElementsOf().foreach {
      a => a.moveAndClick()
    }
  }

  protected def recoverFromErrorPage(): Unit = {
    while ("div[id=serviceErrorPage]".cssElementsOf().nonEmpty) {
      logWarning(
        s"error page encountered, will auto refresh: " +
          s"${"div[id=serviceErrorPage]".cssElementsOf().map(_.getText).mkString("")}")
      Thread.sleep(5000)
      driver.navigate().refresh()
    }
  }

  protected def findAvailableTimes(): Map[String, Seq[WebElement]] = {

    showAllExperiences()

    recoverFromErrorPage()

    waitForPageLoading()

    (for (
      experience <- "div[data-experience-id]".cssElementsOf()
    ) yield {
      "div[class^=name]".cssSingleElementOf(Some(experience)).getText ->
        "div[class^=availableTime]".cssElementsOf(Some(experience))
    }).toMap
  }

  implicit class Retry[T](call: => T) {

    def retry(
      maxRetry: Option[Int] = Some(10),
      callName: Option[String] = None,
      waitFunction: Option[(Int) => Long] = Some(_ => 1000)
    ): Try[T] = {
      retryInternal(0, maxRetry, callName, waitFunction)
    }

    private def retryInternal(
      n: Int,
      maxRetry: Option[Int],
      callName: Option[String],
      waitFunction: Option[(Int) => Long]
    ): Try[T] = {
      Try(call) match {
        case Failure(f) =>
          val waitMs = waitFunction.map(func => func(n)).getOrElse(0L)
          logError(s"call [${callName.getOrElse("")}] " +
            s"encountered exception with ${n} previous retries, max=${maxRetry}, " +
            s"wait=${waitMs}ms: ${f.getMessage}")
          if (maxRetry.map(_ > n).getOrElse(true)) {
            if (waitMs > 0) {
              Thread.sleep(waitMs)
            }
            retryInternal(n + 1, maxRetry, callName, waitFunction)
          } else {
            Failure(f)
          }
        case success => success
      }
    }
  }

  protected def waitWhile(
    condition: => Boolean,
    message: => String,
    timeOutMs: Long = -1
  ): Unit = {
    val initialTime = System.currentTimeMillis()
    var lastPrintTime = 0L
    while (!condition) {
      if (System.currentTimeMillis() - lastPrintTime > 10000) {
        logInfo(s"waited ${System.currentTimeMillis() - initialTime}ms: ${message}")
        lastPrintTime = System.currentTimeMillis()
      }
      Thread.sleep(1000)
      val waitedMs = System.currentTimeMillis() - initialTime
      if (timeOutMs > 0 && waitedMs > timeOutMs) {
        throw new Exception(s"timeout after ${waitedMs}ms for: ${message}")
      }
    }
    logInfo(s"success: ${message}")
  }

  case class ExperienceItems(name: String,
    times: Seq[WebElement]
  ) {
    lazy val matchedName: Boolean = matchName(name)

    lazy val allItems = times.map(time => ExperienceItem(name, time))
    lazy val scoredItems = {
      allItems.filter(
        _.score > 0
      ).sortBy(item => (-item.score, item.itemTime.timeValue))
    }

    override def toString: String = s"${name} at ${times.map(_.getText)}"
  }

  case class ExperienceItem(name: String,
    timeButton: WebElement
  ) {
    val timeString = timeButton.getText

    lazy val itemTime = parseTime(timeButton.getText)
    // HourAndMinute.fromString(timeButton.getText, config.fastPassConfig.timeRegex)

    lazy val score = getScore(name, itemTime)


    override def toString: String = {
      s"${name} at ${timeString}: timeValue=${itemTime.timeValue} score=${score}"
    }
  }

  val matchedExperienceNameStats = collection.mutable.Map.empty[String, LongSummaryStatistics]

  implicit class WebElementConverter(element: WebElement) {
    def moveAndClick(): Unit = {
      logInfo(s"move to ${element.getLocation} and click " +
        s"[${element.getTagName}](${element.getText}): ${element}")
      val wait = new WebDriverWait(driver, 10)
      wait.until(ExpectedConditions.elementToBeClickable(element))
      new Actions(driver).moveToElement(element).click().perform().retry()
      waitForPageLoading()
    }
  }

  def userName: String
  def password: String

  def logIn(): Unit = {
    logInfo("go to login page")
    driver.get("https://disneyworld.disney.go.com/login")
    logInfo("setting user name")
    "input[id=loginPageUsername]".cssSingleElementOf().sendKeys(userName)
    logInfo("setting password")
    "input[id=loginPagePassword]".cssSingleElementOf().sendKeys(password)
    logInfo("sign in")
    "button[id=loginPageSubmitButton]".cssSingleElementOf().click()
  }

  def partyNames: Seq[String]
  def selectParty(): Unit = {
    blockUntil("div[id=selectPartyPage]")
    val upperCaseNames = partyNames.map(_.toUpperCase()).toSet
    def inParty(name: String): Boolean = {
      upperCaseNames.contains(name.toUpperCase())
    }
    val foundGuests = collection.mutable.Set.empty[String]
    def isSelected(guestElement: WebElement): Boolean = {
      val checkmarWrapper = "[aria-hidden]".cssSingleElementOf(Some(guestElement))
      if (checkmarWrapper.getAttribute("aria-hidden") == "true") {
        false
      } else if (checkmarWrapper.getAttribute("aria-hidden") == "false") {
        true
      } else {
        throw new Exception(
          s"${guestElement.getText}, aria-hidden=${checkmarWrapper.getAttribute("aria-hidden")}")
      }
    }

    "div[class^='guest directive']".cssElementsOf().foreach {
      guest =>
        val firstName = "[class^=firstName]".cssSingleElementOf(Some(guest)).getText
        val lastName = "[class^=lastName]".cssSingleElementOf(Some(guest)).getText
        val guestName = s"${firstName} ${lastName}"

        val guestInParty = inParty(guestName)
        if (guestInParty) {
          logInfo(s"${guestName} is in party")
          foundGuests.add(guestName)
        } else {
          logInfo(s"${guestName} not in party")
        }

        if (isSelected(guest)) {
          if (guestInParty) {
            logInfo(s"${guestName} is in party and already selected")
          } else {
            logInfo(s"${guestName} is not in party but selected, unselect")
            guest.moveAndClick()
            if (isSelected(guest)) {
              throw new Exception(s"${guestName} still selected after unselect")
            }
          }
        } else {
          if (guestInParty) {
            logInfo(s"${guestName} is in party but not selected, select")
            guest.moveAndClick()
            if (!isSelected(guest)) {
              throw new Exception(s"${guestName} still not selected after select")
            }
          } else {
            logInfo(s"${guestName} is not in party and not selected")
          }
        }
    }.retry()

    val guestsNotFound = upperCaseNames -- foundGuests.map(_.toUpperCase())
    if (guestsNotFound.nonEmpty) {
      throw new Exception(s"there are guests not found: ${guestsNotFound}")
    }

    val buttons =
      """div[class*=button][role=button]""".cssElementsOf()

    val nextButton = buttons.filter(_.getText == "Next")
    if (nextButton.size != 1) {
      throw new Exception(s"number of next buttons must be exactly 1: ${nextButton.map(_.getText)}")
    }

    nextButton(0).moveAndClick()


    if ("[class=removeGuestDialog]".cssElementsOf().nonEmpty) {
      throw new Exception(s"there are issues with guests in the party")
    }

  }

  def monthSelection: String
  def daySelection: Int
  def parkSelection: String

  def selectDatePark(): Unit = {
    def currentMonth = "[class^=current-month]".cssSingleElementOf()
    logInfo(s"current month is ${currentMonth.getText}")
    if (currentMonth.getText != monthSelection) {
      "[class^=next-month]".cssSingleElementOf().moveAndClick()
      logInfo(s"current month is ${currentMonth.getText} after going to next month")
      if (currentMonth != monthSelection) {
        throw new Exception(
          s"current month ${currentMonth.getText} still not expected ${monthSelection}")
      }
    }
    val availableDays = "[class='day ng-binding ng-scope'][role=link]".cssElementsOf()
    logInfo(s"available days: ${availableDays.map(_.getText)}")
    val selectedDay = availableDays.filter(_.getText.toInt == daySelection)
    if (selectedDay.isEmpty) {
      throw new Exception(s"day ${selectedDay} is not available")
    }
    if (selectedDay.size > 1) {
      throw new Exception(s"multiple days matching selection ${selectedDay.map(_.getText)}")
    }
    selectedDay(0).moveAndClick()

    val parks = "[class^=park][role=button]".cssElementsOf()
    logInfo(s"parks: ${parks.map(_.getText)}")
    val selectedPark = parks.filter(_.getText.toUpperCase().contains(parkSelection.toUpperCase()))
    if (selectedPark.size != 1) {
      throw new Exception(s"selected park must be exactly 1: ${selectedPark.map(_.getText)}")
    }

    selectedPark(0).moveAndClick()
    // there can be park day conflict, force to continue
    continueWithGuests()
  }

  protected def blockUntil(
    cssSelector: String,
    customMessage: Option[String] = None
  ): Unit = {

    def elements = cssSelector.cssOptionalElementOf()

    waitWhile(
      condition = Try {
        recoverFromErrorPage()

        elements.nonEmpty && {
          logInfo(s"${cssSelector} text: ${elements.get.getText}")
          elements.get.getText != ""
        }
      }.toOption.getOrElse(false),

      message = {
        s"${customMessage.getOrElse("")}" +
          s"(signature: ${cssSelector}, found: ${elements.nonEmpty})"
      }
    )
  }

  def continueWithGuests(): Unit = {
    def buttons =
      """div[class*=button][role=button]""".cssElementsOf()

    logInfo(s"found buttons: ${buttons.map(_.getText).mkString(",")}")

    buttons.filter(_.getText.contains("Continue"))
      .foreach {
        continue =>
          continue.moveAndClick()
      }

    buttons.filter(_.getText.contains("Next")).headOption
      .foreach {
        next =>
          next.moveAndClick()
      }
  }

  def selectExperience(): Unit = {
    while ( {

      // hold until finding the selection page
      blockUntilSelectionPage()

      val topMatch = {
        val availableTimes = findAvailableTimes()

        logInfo(s"found ${availableTimes.size} attractions:" +
          s"\n\t${availableTimes.keys.toSeq.sorted.mkString("\n\t")}")

        val allExperiences = availableTimes.map {
          case (name, times) => ExperienceItems(name, times)
        }

        val matchedItems = allExperiences.filter(_.name != "").flatMap(_.scoredItems)
        val currentTime = System.currentTimeMillis()

        val matchedExperiences = allExperiences.filter(_.matchedName)
        matchedExperiences.flatMap(_.allItems).foreach {
          item =>
            matchedExperienceNameStats.getOrElseUpdate(
              s"${item.name} at ${item.timeButton.getText} score=${item.score}",
              new LongSummaryStatistics
            ).accept(currentTime)
        }
        matchedExperienceNameStats.getOrElseUpdate(
          s"___ALL_REFRESHES___", new LongSummaryStatistics).accept(currentTime)

        if (matchedExperiences.isEmpty) {
          logWarning(s"nothing matched the names in the config, " +
            s"make sure the config is right and browser is visible")
        }
        logInfo(s"matchedExperiences=${matchedExperiences}\n" +
          s"matchedItems=${matchedItems.size}: ${matchedItems}")
        matchedItems.headOption

      }.retry() match {
        case Success(result) => result
        case Failure(f) => throw new Exception(s"failed to find available times after retry: ${f}")
      }

      val itemsStatsMap =
        matchedExperienceNameStats.map {
          case (id, stats) => id -> Map(
            "first" -> s"${(System.currentTimeMillis() - stats.getMin) / 1000} ago",
            "last" -> s"${(System.currentTimeMillis() - stats.getMax) / 1000} ago",
            "total" -> stats.getCount
          )
        }
      logInfo(s"accumulative stats:\n\t${itemsStatsMap.toSeq.sortBy(_._1).mkString("\n\t")}")
      topMatch match {
        case Some(item) =>

          logInfo(s"selected ${item}")
          item.timeButton.moveAndClick()

          def findConfirm = {
            continueWithGuests()
            def buttons =
              """div[class*=button][role=button]""".cssElementsOf()
                .retry().getOrElse(throw new Exception(s"failed to get buttons after retry"))

            logInfo(s"found buttons: ${buttons.map(_.getText).mkString(",")}")

            val confirm = buttons.filter(_.getText == "Confirm").headOption.getOrElse(
              throw new Exception(s"didn't find confirm")
            )
            confirm
          }

          val confirm = findConfirm.retry().getOrElse(throw new Exception(s"failed to get confirm"))

          val toBeCancelled = "div[class^=canceledAttraction]".cssOptionalElementOf()
          toBeCancelled.map {
            cancel =>
              val name =
                "div[class^=attractionNameContainer]".cssSingleElementOf(Some(cancel)).getText
              val window = "div[class^=arrivalWindow]".cssSingleElementOf(Some(cancel))
              val cancelItem = new ExperienceItem(name, window)
              logInfo(s"to be cancelled: ${cancelItem}")
              if (item.score < cancelItem.score ||
                item.score == cancelItem.score && !(item.itemTime < cancelItem.itemTime)) {
                throw new Exception(
                  s"there is no value to select ${item} and cancel ${cancelItem}, " +
                    s"may need to manually adjust scoring function " +
                    s"if it's not aware of what has already been selected"
                )
              }
          }
          confirm.moveAndClick()


          logInfo(s"Successfully confirmed ${item}")

          false

        case None =>
          logInfo(s"no match found, refresh")
          driver.navigate().refresh()
          logInfo(s"refresh finished")
          Thread.sleep(1000)
          true
      }
    }
    ) {

    }

  }
}
