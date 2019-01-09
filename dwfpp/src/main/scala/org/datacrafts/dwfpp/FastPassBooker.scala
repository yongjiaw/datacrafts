package org.datacrafts.dwfpp

import java.util.LongSummaryStatistics

import scala.reflect.internal.FatalError
import scala.util.{Failure, Success, Try}

import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}

import org.datacrafts.logging.Slf4jLogging

trait FastPassBooker extends Slf4jLogging.Default {

  def driver: WebDriver

  // def config: FastPassBookerConfig

  def selectionPageSignature: String

  def attractionHasValue(name: String): Boolean

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
        throw new FatalError(
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
      selectionPageSignature,
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
        case Failure(f: FatalError) =>
          logError(s"call [${callName.getOrElse("")}] " +
            s"encountered fatal error and won't retry: ${f.getMessage}")
          Failure(f)
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
    timeOutMs: Long = 1000L * 30
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
    lazy val hasValue: Boolean = attractionHasValue(name)

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

  def login(): Unit = {
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

    val names =
    "div[class^='guest directive']".cssElementsOf().map {
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
        guestName
    }.retry(callName = Some("select guests"))

    logInfo(s"all guests in the account: ${names}")

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

  def getState(): Any = {}

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

  // after fast pass is full, can only use modify
  // or cancel some fast pass and restart
  def fastPassFull: Boolean = _fastPassFull

  private var _fastPassFull: Boolean = false

  def continueWithGuests(): Unit = {
    def buttons =
      """div[class*=button][role=button]""".cssElementsOf()

    logInfo(s"found buttons: ${buttons.map(_.getText).mkString(",")}")

    val removeButtons = buttons.filter(_.getText.contains("Remove"))
    val continueButtons = buttons.filter(_.getText.contains("Continue"))

    if (removeButtons.nonEmpty && continueButtons.isEmpty) {
      _fastPassFull = true
      // throw FatalError(s"Reached fast pass limit, need to modify existing passes")
      // if fast pass if full, the only option is to modify
      // exiting fast pass with the same guests and in the same park
      logInfo(s"fast pass is full, find a fast pass to modify")
      selectFastpassToModify()
    }
    else {

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
    }.retry(callName = Some("continue with guest"))
  }

  var _fastPassModifyPointer = 0

  def addSelection(attractionSelection: AttractionSelection): Unit
  def selectFastpassToModify(): Unit = {
    driver.get("https://disneyworld.disney.go.com/fastpass-plus/")
    val listings = "div[class='listing ng-scope']".cssElementsOf()
    val selectedDay = s"${monthSelection} ${daySelection}"
    val matchedListingWithSelection =
    for (
      listing <- listings;
      monthAnyYear = "[class^=monthAndYear]".cssSingleElementOf(Some(listing)) if {
        val matched = monthAnyYear.getText.contains(selectedDay)
        logInfo(
          s"checking fast pass for ${monthAnyYear}, matched=${matched}: selection=${selectedDay}")
        matched
      }
    ) yield {

      val selections = "div[role=button][class^=entitlement]".cssElementsOf(Some(listing))
      if (selections.isEmpty) {
        throw FatalError(s"no selections found for ${monthAnyYear.getText}")
      }

      val validSelections =
      for (
        selection <- selections if {
          selection.moveAndClick()
          val guests = "[class=guest-card__info]".cssElementsOf()
          if (guests.isEmpty) {
            throw FatalError(s"didn't find guest info")
          }
          val names =
            for (guest <- guests) yield {

              val names = "[class^=ng-binding]".cssElementsOf(Some(guest))
              if (names.size != 2) {
                throw FatalError(
                  s"guest must have first and last names, found: ${names.map(_.getText)}")
              }
              names.map(_.getText).mkString(" ")
            }
          // close the pop up
          "div[class^='icon close'][role=button]".cssSingleElementOf().moveAndClick()

          val containsAll = partyNames.forall(names.contains)
          logInfo(s"containsAll=${containsAll} for ${selection.getText}")
          containsAll
        }
      ) yield {
        val attractionName = "h3".cssSingleElementOf(Some(selection)).getText
        logInfo(s"attraction=${attractionName}")
        val arrivalTime = "div[class^=arrivalItemTime]".cssSingleElementOf(Some(selection)).getText
        logInfo(s"arrivalTime=${attractionName}")
        val attractionSelection =
        AttractionSelection.fromNameAndTime(attractionName, parseTime(arrivalTime))
        addSelection(attractionSelection)
        (selection, attractionSelection)
      }

      if (validSelections.isEmpty) {
        throw FatalError(
          s"no existing fast pass contains all the parties ${partyNames}, " +
            s"must manually cancel some fastpass," +
            s" or modify multiple fastpasses to make them available")
      }

      val selectionIndex = _fastPassModifyPointer % selections.size
      _fastPassModifyPointer += 1


      val selection = validSelections(selectionIndex)

      monthAnyYear.getText -> selection
    }

    if (matchedListingWithSelection.isEmpty ||
      matchedListingWithSelection.size > 1) {
      throw FatalError(
        s"must have exactly one matching for ${selectedDay}: ${matchedListingWithSelection.map(_._1)}")
    }

    val selection = matchedListingWithSelection(0)._2
    logInfo(s"modifying ${selection}")

    selection._1.moveAndClick()

    val buttons = "div[role=button][class^=clickable]".cssElementsOf()
    val modifyButtons = buttons.filter(_.getText == "Modify")
    if (modifyButtons.isEmpty) {
      throw FatalError(s"did not find modify button")
    }
    if (modifyButtons.size > 1) {
      throw FatalError(s"more than 1 modify buttons: ${modifyButtons.size}")
    }
    modifyButtons(0).moveAndClick()

    modifiedItem = Some(selection._2)

    selectParty()
  }

  var modifiedItem: Option[AttractionSelection] = None

  def evaluateAction(
    selection: ExperienceItem,
    toBeCancelled: Seq[WebElement]
  ): Boolean = {
    logInfo(s"to be canceled: ${toBeCancelled.map(_.getText)}")
    if (toBeCancelled.nonEmpty) {
      val maxCancel = toBeCancelled.map {
        cancel =>
          val name =
            "div[class^=attractionNameContainer]".cssSingleElementOf(Some(cancel)).getText
          val window = "div[class^=arrivalWindow]".cssSingleElementOf(Some(cancel))
          new ExperienceItem(name, window)
      }.maxBy(_.score)
      logInfo(s"max cancel: ${maxCancel}")
      if (selection.score < maxCancel.score) {
        logInfo(
          s"there is no value to select ${selection} and cancel ${maxCancel}, " +
            s"may need to manually adjust scoring function " +
            s"if it's not aware of what has already been selected"
        )
        false
        /* throw new Exception(
          s"there is no value to select ${selection} and cancel ${maxCancel}, " +
            s"may need to manually adjust scoring function " +
            s"if it's not aware of what has already been selected"
        ) */
      } else {
        logInfo(s"there more value to select ${selection} and cancel ${maxCancel}")
        true
      }
    } else {
      logInfo(s"select ${selection}, nothing to cancel")
      true
    }
  }

  def updateSelection(
    selection: ExperienceItem,
    toBeCancelled: Seq[WebElement],
    confirmed: Boolean
  ): Unit = {

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

        val matchedExperiences = allExperiences.filter(_.hasValue)
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

      }.retry(callName = Some("finding top matches")) match {
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

            logInfo(s"found buttons: ${buttons.map(_.getText).mkString(",")}")

            val confirm = buttons.filter(_.getText == "Confirm").headOption.getOrElse(
              throw new Exception(s"didn't find confirm")
            )
            confirm
          }

          val confirm = findConfirm.retry(
            callName = Some(s"find confirm after select: ${item}")
          ).getOrElse(throw new Exception(s"failed to get confirm"))

          val toBeCancelled =
            "div[class^=canceledAttraction]".cssOptionalElementOf()
              .map {
                cancel =>
                  val attractions = "div[class=attraction]".cssElementsOf(Some(cancel))
                  if (attractions.isEmpty) throw new FatalError(
                    s"cancel attractions container must contain attractions"
                  )
                  attractions
              }.getOrElse(Seq.empty)
          if (
            evaluateAction(item, toBeCancelled)
          ) { // confirm and get ready for next
            confirm.moveAndClick()

            logInfo(s"Successfully confirmed ${item}")

            updateSelection(item, toBeCancelled, true)

            val sameDayButton = "div[action-button^=sameDay]".cssSingleElementOf()
            if (sameDayButton.getAttribute("class").endsWith("disabled")) {
              logInfo(s"sameDay button is disabled, find a fast pass to modify")
              selectFastpassToModify()

            } else {
              logInfo(s"keep booking for the same day")
              sameDayButton.moveAndClick()
              val buttons =
                """div[class*=button][role=button]""".cssElementsOf()

              val nextButton = buttons.filter(_.getText == "Next")
              if (nextButton.size != 1) {
                throw new Exception(
                  s"number of next buttons must be exactly 1: ${nextButton.map(_.getText)}")
              }

              nextButton(0).moveAndClick()
            }

          } else {
            updateSelection(item, toBeCancelled, false)
            // go back
            val buttons =
              """div[class*=button][role=button]""".cssElementsOf()

            val backButton = buttons.filter(_.getText == "Back")
            if (backButton.size != 1) {
              throw new Exception(
                s"number of next buttons must be exactly 1: ${backButton.map(_.getText)}")
            }

            backButton(0).moveAndClick()
          }

          // now expect the selection page
          // selectionPageSignature.cssSingleElementOf()

          true

        case None =>
          logInfo(s"no match found, refresh")
          driver.navigate().refresh()
          logInfo(s"refresh finished")
          Thread.sleep(1000)
          true
      }
    }
    ) {
      logInfo(s"currentState: ${getState()}")
    }

  }

  def matchAttraction(attractionName: String): AttractionWithParkLand

  object AttractionSelection {
    def fromExperience(selection: ExperienceItem): AttractionSelection = {
      new AttractionSelection(
        matchAttraction(selection.name),
        parseTime(selection.timeString)
      )
    }
    def fromNameAndTime(name: String, time: HourAndMinute): AttractionSelection = {
      new AttractionSelection(
        matchAttraction(name),
        time
      )
    }
    def fromWebElement(element: WebElement): AttractionSelection = {
      val name =
        "div[class^=attractionNameContainer]".cssSingleElementOf(Some(element)).getText
      val window = "div[class^=arrivalWindow]".cssSingleElementOf(Some(element))

      new AttractionSelection(matchAttraction(name), parseTime(window.getText))

    }
  }
}
