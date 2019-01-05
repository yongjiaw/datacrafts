package org.datacrafts.dwfpp

import java.util.LongSummaryStatistics

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.interactions.{Action, Actions}

import org.datacrafts.logging.Slf4jLogging

trait FastPassBooker extends Slf4jLogging.Default {

  def driver: WebDriver

  def config: FastPassBookerConfig

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
      cssElementsOf(parent).headOption

    }

    def cssSingleElementOf(parent: Option[WebElement] = None): WebElement = {
      val elements = cssElementsOf(parent)
      if (elements.size != 1) {
        throw new Exception(
          s"css selector [${value}] must have exactly one match: size=${elements.size}")
      } else {
        elements.head
      }
    }
  }

  protected def blockUntilSelectionPage(): Unit = {
    val pageSelector = config.fastPassConfig.landingPageSignature

    def elements = pageSelector.cssOptionalElementOf()

    waitWhile(
      condition = Try {
        recoverFromErrorPage()

        elements.nonEmpty && {
          logInfo(s"selectExperiencePage text: ${elements.get.getText}")
          elements.get.getText != ""
        }
      }.toOption.getOrElse(false),

      message = {
        s"required interactive steps before automation (never change font size): go to browser, " +
          s"login, select party, select date, select park, " +
          s"click desirable day part (Morning, Afternoon, Evening), " +
          s"(signature: ${pageSelector}, found: ${elements.nonEmpty})"
      }
    )
  }

  protected def showAllExperiences(): Unit = {
    "a[class^=showMore]".cssElementsOf().foreach {
      a =>
        logInfo(s"clicking ${a.getText}")
        new Actions(driver).moveToElement(a).click().perform()
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
      maxRetry: Option[Int] = None,
      callName: Option[String] = None,
      waitFunction: Option[(Int) => Long] = None
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
    lazy val matchedName: Boolean = {
      config.fastPassConfig.priorities.exists(
        detail => name.matches(detail.namePattern)
      )
    }

    lazy val allItems = times.map(time => ExperienceItem(name, time))
    lazy val rankedItems = {
      allItems.filter(
        _.rank > -1
      ).sortBy(item => (item.rank, item.itemTime.timeValue))
    }
  }

  case class ExperienceItem(name: String,
    timeButton: WebElement
  ) {
    lazy val itemTime =
      HourAndMinute.fromString(timeButton.getText, config.fastPassConfig.timeRegex)

    lazy val rank = {
      (0 until config.fastPassConfig.priorities.size).collectFirst {
        case i if {
          val ExperienceSelectionConfig(namePattern, timeSlot) = config.fastPassConfig.priorities(i)
          name.matches(namePattern) && timeSlot.contains(itemTime)
        } => i
      }.getOrElse(-1)
    }

    override def toString: String = {
      s"${name} at ${timeButton.getText}: timeValue=${itemTime.timeValue} rank=${rank}"
    }
  }

  val matchedExperienceNameStats = collection.mutable.Map.empty[String, LongSummaryStatistics]

  def selectExperience(): Unit = {
    while ( {
      val topMatch = {
        val availableTimes = findAvailableTimes()

        val allExperiences = availableTimes.map {
          case (name, times) => ExperienceItems(name, times)
        }

        val matchedItems = allExperiences.flatMap(_.rankedItems)
        val currentTime = System.currentTimeMillis()
        val matchedExperiences = allExperiences.filter(_.matchedName)
        matchedExperiences.flatMap(_.allItems).foreach {
          item =>
            matchedExperienceNameStats.getOrElseUpdate(
              s"${item.name} at ${item.timeButton.getText} rank=${item.rank}",
              new LongSummaryStatistics
            ).accept(currentTime)
        }
        matchedExperienceNameStats.getOrElseUpdate(
          s"___ALL_REFRESHES___", new LongSummaryStatistics).accept(currentTime)

        if (matchedExperiences.isEmpty) {
          logWarning(s"nothing matched the names in the config, " +
            s"make sure the config is right and browser is visible")
        }
        logInfo(s"matchedExperiences=${matchedExperiences}, " +
          s"matchedItems=${matchedItems.size}: ${matchedItems}")
        matchedItems.headOption

      }.retry(maxRetry = Some(10)) match {
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
          item.timeButton.click()
          logInfo(s"selected ${item}")
          false
        case None =>
          logInfo(s"no match found, refresh")
          driver.navigate().refresh()
          Thread.sleep(1000)
          true
      }
    }
    ) {

    }

  }
}
