package org.datacrafts.app.dwfpp

import scala.reflect.internal.FatalError
import scala.util.{Failure, Random, Success, Try}

import com.sun.net.httpserver.Authenticator.Success
import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.support.ui.{ExpectedConditions, Select, WebDriverWait}

import org.datacrafts.app.dwfpp.FastPassGreedyBooker.FastPassChanges
import org.datacrafts.dwfpp.{AttractionWithParkLand, Config, ConfigParser, HourAndMinute}
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.util.Retriable

trait WebDriverClient extends Retriable {

  Self: Slf4jLogging =>

  def createDriver(): WebDriver

  private var _driver: Option[WebDriver] = None

  def driver: WebDriver = {
    val d = _driver.getOrElse({
      logInfo(s"creating driver")
      createDriver()
    })
    _driver = Some(d)
    d
  }

  def closeDriver(): Unit = {
    Try {
      _driver.foreach(_.quit())
      _driver.foreach(_.close())
    }
    _driver = None
  }

  def login(userName: String,
    password: String
  ): Unit = {
    logInfo("go to login page")
    driver.get("https://disneyworld.disney.go.com/login")
    logInfo("setting user name")
    "input[id=loginPageUsername]".cssSingleElementOf().sendKeys(userName)
    logInfo("setting password")
    "input[id=loginPagePassword]".cssSingleElementOf().sendKeys(password)
    logInfo("sign in")
    "button[id=loginPageSubmitButton]".cssSingleElementOf().moveAndClick()
  }

  def gotoPartySelection(): Unit = {
    driver.get("https://disneyworld.disney.go.com/fastpass-plus/select-party/")
  }

  def selectParty(guestNames: String*): Unit = {

    val upperCaseNames = guestNames.map(_.toUpperCase()).toSet

    def inParty(name: String): Boolean = {
      upperCaseNames.contains(name.toUpperCase())
    }

    val foundGuests = collection.mutable.Set.empty[String]

    def isSelected(guestElement: WebElement): Boolean = {
      val checkmarWrapper = "[aria-hidden]".cssSingleElementOf(guestElement)
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
          val firstName = "[class^=firstName]".cssSingleElementOf(guest).getText
          val lastName = "[class^=lastName]".cssSingleElementOf(guest).getText
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
      }.withRetry(callName = "select guests")

    logInfo(s"all guests in the account: ${names}")

    val guestsNotFound = upperCaseNames -- foundGuests.map(_.toUpperCase())
    if (guestsNotFound.nonEmpty) {
      throw new Exception(s"there are guests not found: ${guestsNotFound}")
    }

    val buttons =
      """div[class*=button][role=button]""".cssElementsOf()

    buttons.singleFilteredElement(_.getText == "Next").moveAndClick()

    if ("[class=removeGuestDialog]".cssElementsOf().nonEmpty) {
      throw new Exception(s"there are issues with guests in the party")
    }

  }

  def selectDatePark(
    monthSelection: String,
    daySelection: Int,
    parkSelection: String
  ): Unit = {
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
    val availableDays = "[class^='day ng-binding ng-scope'][role=link]".cssElementsOf()
    logInfo(s"available days: ${availableDays.map(_.getText)}")

    availableDays.singleFilteredElement(
      day => Try(day.getText.toInt).toOption.map(_ == daySelection).getOrElse(false)
    ).moveAndClick()

    {
      val parks = "[class^=park][role=button]".cssElementsOf()
      logInfo(s"parks: ${parks.map(_.getText)}")
      parks.singleFilteredElement(
        _.getText.toUpperCase().contains(parkSelection.toUpperCase())
      ).moveAndClick()
    }.withRetry(callName = "select park")

    // there can be park day conflict, force to continue

  }

  // all guests are fine, this is to continue after park date selection
  def continueWithGuestsAfterParkDateSelection(): Unit = {
    continueWithGuest()

    """div[class*=button][role=button][aria-label='button.next'][aria-disabled=false]"""
      .cssOptionalElementOf().map(_.moveAndClick()).getOrElse(
      isSelectionOrError()
    )


  }

  def parseFastPasses(
    parks: Seq[Config.Park]
  ): Map[String, Seq[(FastPass, WebElement)]] = {

    // driver.get("https://disneyworld.disney.go.com/fastpass-plus/")
    val listings = "div[class='listing ng-scope']".cssElementsOf()

    val fastPassesMap: Map[String, Seq[(FastPass, WebElement)]] =
      listings.map {
        listing =>
          val monthAnyYear = "[class^=monthAndYear]".cssSingleElementOf(listing)
          val selections = "div[role=button][class^=entitlement]".cssElementsOf(listing)
          if (selections.isEmpty) {
            throw FatalError(s"no selections found for ${monthAnyYear.getText}")
          }

          val fastPasses: Seq[(FastPass, WebElement)] =
            for (
              selection <- selections
            ) yield {
              // pop up the details and parse information
              selection.moveAndClick()
              val guests = "[class=guest-card__info]".cssElementsOf()
              if (guests.isEmpty) {
                throw FatalError(s"didn't find guest info")
              }
              val names =
                for (guest <- guests) yield {
                  val names = "[class^=ng-binding]".cssElementsOf(guest)
                  if (names.size != 2) {
                    throw FatalError(
                      s"guest must have first and last names, found: ${names.map(_.getText)}")
                  }
                  names.map(_.getText).mkString(" ")
                }
              logInfo(s"guests=${guests}")
              val attractionName = "h3".cssSingleElementOf(selection).getText
              logInfo(s"attraction=${attractionName}")
              val arrivalTime = "div[class^=arrivalItemTime]".cssSingleElementOf(selection).getText
              logInfo(s"arrivalTime=${arrivalTime}")

              // close the pop up
              {
                if ("[class=guest-card__info]".cssElementsOf().nonEmpty) {
                  "div[class^='icon close'][role=button]".cssSingleElementOf().moveAndClick()
                  throw new Exception(s"guest info presents, clicked close icon")
                }
              }.withRetry(maxRetry = 10)

              (
                FastPass(
                  AttractionWithParkLand.matchAttraction(parks, attractionName),
                  HourAndMinute.fromString(arrivalTime),
                  names.toSet
                ),
                selection
              )
            }.withRetry()

          monthAnyYear.getText -> fastPasses
      }.toMap
    fastPassesMap
  }

  def parseCancellingItems(parks: Seq[Config.Park]): Seq[FastPass] = {
    "div"

    "div[class^=canceledAttraction]".cssOptionalElementOf()
      .map {
        cancel =>
          val attractions = "div[class=attraction]".cssElementsOf(cancel)
          if (attractions.isEmpty) throw new FatalError(
            s"cancel attractions container must contain attractions"
          )
          attractions.map {
            attraction =>
              val name =
                "div[class^=attractionNameContainer]".cssSingleElementOf(attraction).getText
              if (name == "") {
                throw new Exception(s"cancel item detail not loaded yet")
              }
              logInfo(s"cancelling item: ${name}")
              val window = "div[class^=arrivalWindow]".cssSingleElementOf(attraction)
              val guests = "div[class^=nameOfGuests]".cssSingleElementOf(attraction)
              val names = "div[class^=guest]".cssElementsOf(guests).map(_.getText)
              if (window.getText == "") {
                throw new Exception(s"cancel item detail not loaded yet")
              }
              FastPass(
                AttractionWithParkLand.matchAttraction(parks, name),
                HourAndMinute.fromString(window.getText),
                names.toSet
              )

          }.withRetry()
      }.getOrElse(Seq.empty)
  }

  def reachedErrorPage(): Boolean = {
    "div[id=serviceErrorPage]".cssOptionalElementOf() match {
      case Some(element) =>
        logInfo(s"error page: ${element.getText}")
        true
      case None => false
    }
  }


  def isSelectionOrError(): (Boolean, Boolean) = {
    val isSelectionPage = reachedSelectionPage()
    val isErrorPage = reachedErrorPage()
    if (!isSelectionPage && !isErrorPage) {
      throw new Exception(s"unrecognized page")
    }
    if (isSelectionPage && isErrorPage) {
      throw new FatalError(s"cannot be both selection and error page")
    }
    (isSelectionPage, isErrorPage)
  }.withRetry(callName = "checking page selection or error")

  def reachedSelectionPage(): Boolean = {
    "div[id=selectExperiencePage]".cssOptionalElementOf() match {
      case Some(element) =>
        val title = "[id=headerTitle]".cssSingleElementOf(element).getText.withRetry()
        logInfo(s"select experience: $title")
        if (title == "") {
          throw new Exception(s"selection title is empty")
        }

        val text = "[class=experienceNameLand]".cssElementsOf().map(_.getText).mkString("")
        if (text.isEmpty) {
          throw new Exception(s"experience text is empty")
        }
        true
      case None => false
    }
  }.withRetry()

  def fastPassLimitReachedBeforeSelection(): Boolean = {
    // there are 2 situations to detect this
    // 1. after selecting park and date
    // 2. after confirming a fast pass
    "div[class^='guestWithConflicts container'][aria-hidden=false]".cssElementsOf().exists {
      conflict =>
        val buttons =
          """div[class*=button][role=button]""".cssElementsOf(conflict)
        logInfo(s"found buttons: ${buttons.map(_.getText).mkString(",")}")
        val firstName = "[class^=firstName]".cssSingleElementOf(conflict).getText
        val lastName = "[class^=lastName]".cssSingleElementOf(conflict).getText
        val name = s"$firstName $lastName"
        val conflictTitle = "div[class^=conflictTitle]".cssSingleElementOf(conflict).getText

        val reachedLimit =
          buttons.optionalFilteredElement(_.getText.contains("Continue")).isEmpty &&
            buttons.optionalFilteredElement(_.getText.contains("Remove")).nonEmpty
        logInfo(s"reachedLimit=${reachedLimit}: ${name}, ${conflictTitle}")
        reachedLimit
    }
  }

  def fastPassLimitedReachedAfterSelection(): Boolean = {
    val sameDay = "[action-button^=sameDay]".cssSingleElementOf()
    if (sameDay.getAttribute("enabled") == "true") {
      false
    } else {
      true
    }
  }

  def modifyRandomFastPass(
    previousModify: Option[FastPass],
    parks: Seq[Config.Park],
    monthSelection: String,
    daySelection: Int,
    guests: Seq[String]
  ): FastPass = {
    driver.get("https://disneyworld.disney.go.com/fastpass-plus/")

    {
      val fastPassesMap = parseFastPasses(parks)
      val matchedFastPasses =
        for (
          (monthAnyYear, fastPasses) <- fastPassesMap
          if monthAnyYear.contains(s"${monthSelection} $daySelection");
          (fastPass, element) <- fastPasses
          if {
            guests.forall(
              fastPass.party.contains
            )
          }
        ) yield {
          (fastPass, element)
        }

      if (matchedFastPasses.isEmpty) {
        if (fastPassesMap.isEmpty) {
          throw new Exception(s"no fast pass found")
        }
        else {
          throw FatalError(s"" +
            s"no existing fast pass to modify for ${guests} on ${monthSelection} ${daySelection}"
          )
        }
      }
      val randIndex = Random.nextInt(matchedFastPasses.size)
      logInfo(s"found ${matchedFastPasses.size} fast passes to modify " +
        s"and pick randonIndex=${randIndex}")

      val sorted = matchedFastPasses.toSeq.sortBy(_._1.attraction.attraction.name)
      val (fastPass, element) = sorted(randIndex)
      logInfo(s"modifying $fastPass")
      element.moveAndClick()

      "div[role=button][class^=clickable]".cssElementsOf()
        .singleFilteredElement(_.getText == "Modify").moveAndClick()

      {
        "[role=button]".cssElementsOf().singleFilteredElement(_.getText == "Select All")
          .moveAndClick()
      }.withRetry()

      {
        """div[class*=button][role=button][aria-label='button.next'][aria-disabled=false]"""
          .cssSingleElementOf().moveAndClick()
      }.withRetry()

      fastPass
    }.withRetry()
  }.withRetry()

  def confirmSelection(
  ): Unit = {

    """div[class*=button][role=button][aria-label='button.confirm'][aria-disabled=false]"""
      .cssSingleElementOf().moveAndClick().withRetry(maxRetry = 5)

    // wait for confirming text to disappear, ok to fail
    Try({
      if (
        """div[class^=loadingText]"""
          .cssElementsOf()
          .filteredElements(_.getText.contains("Confirm")).nonEmpty
      ) {
        throw new Exception()
      }

    }.withRetry()
    )

  }

  def continueWithGuest(): Unit = {
    "div[class^='guestWithConflicts container'][aria-hidden=false]".cssElementsOf().foreach {
      conflict =>
        val buttons =
          """div[class*=button][role=button]""".cssElementsOf(conflict)
        logInfo(s"found buttons: ${buttons.map(_.getText).mkString(",")}")
        val firstName = "[class^=firstName]".cssSingleElementOf(conflict).getText
        val lastName = "[class^=lastName]".cssSingleElementOf(conflict).getText
        val name = s"$firstName $lastName"
        if (firstName.isEmpty && lastName.isEmpty) {
          throw new Exception(s"empty guest name")
        }
        val conflictTitle = "div[class^=conflictTitle]".cssSingleElementOf(conflict).getText

        buttons.optionalFilteredElement(_.getText.contains("Continue")) match {
          case Some(button) => button.moveAndClick()
          case None =>
            throw FatalError(s"${name} has conflict and cannot continue: ${conflictTitle}")
        }
    }
  }.withRetry()

  def waitForLoader(): Unit = {
    val loaders = driver.findElements(By.cssSelector("div[id=loader]"))
    if (loaders.size() > 1) {
      throw FatalError(s"more than 1 loaders")
    }
    if (loaders.size() == 1) {
      throw new Exception("still loading")
    }
  }.withRetry(callName = "loader waiter")

  def selectTimeValue(time: String): Unit = {
    val selectBox = "div[role=button][class^='select-box clickable']".cssSingleElementOf()
    val selectText = "[class^='select-text']".cssSingleElementOf(selectBox).getText
    if (selectText.isEmpty) {
      throw new Exception(s"selection text is empty")
    }
    if (selectText.contains(time)) {
      logInfo(s"${time} is already selected: ${selectText}")
    } else {
      selectBox.moveAndClick()
      logInfo(s"selecting ${time}, current selection is ${selectText}")
      "div[role=button][class^=option]".cssElementsOf()
        .singleFilteredElement(_.getText.contains(time)).moveAndClick()
    }
  }.withRetry()

  def selectTimeFilter(time: String): Unit = {
    {
      val timeFilter = "div[class^='timeFilterOption']".cssElementsOf()
        .singleFilteredElement(_.getText == time)

      if (timeFilter.getAttribute("class").contains("selected")) {
        logInfo(s"${time} already selected")
      } else {
        logInfo(s"${time} not selected, clicking")
        "[role=button]".cssSingleElementOf(timeFilter).moveAndClick()
      }
    }.withRetry()
  }
  def continueWithGuestBeforeConfirm(): Unit = {
    {
      continueWithGuest()
      """div[class*=button][role=button][aria-label='button.next'][aria-disabled=false]"""
        .cssOptionalElementOf().map(_.moveAndClick())
      """div[class*=button][role=button][aria-label='button.confirm'][aria-disabled=false]"""
        .cssSingleElementOf()
    }.withRetry()
  }

  def collectionCurrentDayFastPass(
    month: String,
    day: Int,
    parks: Seq[Config.Park]
  ): Seq[FastPass] = {
    driver.get("https://disneyworld.disney.go.com/fastpass-plus/")
    waitForFastPassPageLoad()
    val fastPasses = parseFastPasses(parks)
      .filter {
        _._1.contains(s"${month} ${day}")
      }.flatMap(_._2.map(_._1))
    logInfo(s"${fastPasses.size} confirmed fast passes for ${month} ${day}")

    fastPasses.toSeq

  }

  def waitForFastPassPageLoad(): Unit = {
    {
      val addButtons = "[role=button]".cssElementsOf()
        .filteredElements(_.getText.contains("Add Fast"))
      val getStarted = "[role=button]".cssElementsOf()
        .filteredElements(_.getText.contains("Get Start"))
      if (addButtons.isEmpty && getStarted.isEmpty) {
        throw new Exception(s"FastPass page not loaded yet")
      }
    }.withRetry()
  }

  def moveAndClick(webElement: WebElement): Unit = {
    webElement.moveAndClick()
  }

  def getAvailableFastPassSelection(
    parks: Seq[Config.Park],
    guestNames: Seq[String]
  ): Seq[(AttractionWithParkLand, Seq[(FastPass, WebElement)])] = {
    // click show more experiences
    "a[class^=showMore]".cssElementsOf().foreach {
      a => a.moveAndClick()
    }
    for (
      experience <- "div[data-experience-id]".cssElementsOf();
      name = "div[class^=name]".cssSingleElementOf(experience).getText;
      attraction = AttractionWithParkLand.matchAttraction(parks, name)
    ) yield {

      val times = "div[class^=availableTime]".cssElementsOf(experience);
      attraction -> times.map {
        time =>
          (
            FastPass(
              attraction,
              HourAndMinute.fromString(time.getText),
              guestNames.toSet
            ),
            time
          )
      }
    }
  }.withRetry()

  implicit class WebElementConverter(element: WebElement) {
    def moveAndClick(): Unit = {
      logInfo(s"move to ${element.getLocation} and click " +
        s"[${element.getTagName}](${element.getText}): ${element}")
      val wait = new WebDriverWait(driver, 10)
      wait.until(ExpectedConditions.elementToBeClickable(element))
      new Actions(driver).moveToElement(element).click().perform().withRetry()
    }
  }


  implicit class CssElementsConverter(elements: Seq[WebElement]) {

    def filteredElements(filter: WebElement => Boolean): Seq[WebElement] = {
      elements.filter(filter)
    }

    def optionalFilteredElement(filter: WebElement => Boolean): Option[WebElement] = {
      val filtered = filteredElements(filter)
      if (filtered.size > 1) {
        throw new FatalError(
          s"optional filtered element cannot have more than one match size=${elements.size}: ${elements}")
      }
      filtered.headOption
    }

    def singleFilteredElement(filter: WebElement => Boolean): WebElement = {
      optionalFilteredElement(filter).getOrElse(
        throw new Exception(
          s"single filtered element must have a match"
        )
      )
    }
  }

  private var currentPageSource = ""

  implicit class StringConverter(value: String) {

    import scala.collection.JavaConverters._

    def cssElementsOf(
      parent: WebElement = null
    ): Seq[WebElement] = {

      waitForLoader()

      {
        if (driver.getPageSource != currentPageSource) {
          logDebug(s"page still loading ${currentPageSource.size} -> ${driver.getPageSource.size}")
          currentPageSource = driver.getPageSource
        }
      }.withRetry(
        maxWait = 1000 * 30
      )
      logDebug(s"page finished loading ${currentPageSource.size}")

      Option(parent) match {
        case Some(p) => p.findElements(By.cssSelector(value)).asScala
        case None => driver.findElements(By.cssSelector(value)).asScala
      }
    }

    def cssOptionalElementOf(
      parent: WebElement = null
    ): Option[WebElement] = {
      val elements = cssElementsOf(parent)
      if (elements.size > 1) {
        throw new FatalError(
          s"css selector [${value}] cannot have more than one match size=${elements.size}: " +
            s"${elements.map(_.getText)}")
      }
      elements.headOption

    }

    def cssSingleElementOf(
      parent: WebElement = null
    ): WebElement = {
      cssOptionalElementOf(parent).getOrElse(
        throw new Exception(
          s"css selector [${value}] must have exactly one match")
      )
    }
  }

}
