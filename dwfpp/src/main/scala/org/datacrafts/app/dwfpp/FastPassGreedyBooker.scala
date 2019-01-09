package org.datacrafts.app.dwfpp

import java.io.{File, FileOutputStream}
import java.util.{Calendar, TimeZone}

import scala.annotation.tailrec
import scala.reflect.internal
import scala.reflect.internal.FatalError
import scala.util.{Failure, Success, Try}

import org.openqa.selenium.WebElement

import org.datacrafts.app.dwfpp.FastPassGreedyBooker.{AttractionStatsMap, EvaluationWithMetadata, FastPassChanges, MatchedAttractionStatsMap}
import org.datacrafts.dwfpp.{AttractionWithParkLand, Config, HourAndMinute, UnknownAttraction}
import org.datacrafts.dwfpp.Config.Attraction
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.json.{JsonOperation, JsonOperationDsl}
import org.datacrafts.util.Retriable

class ChromeGreedyBooker(config: Config) extends FastPassGreedyBooker(config) {
  override lazy val client: WebDriverClient = ChromeClient
}

abstract class FastPassGreedyBooker(config: Config)
  extends Slf4jLogging.Default with Retriable with JsonOperationDsl {

  lazy val logRoot: String = {
    Option(config.logRoot).getOrElse("log")
  }

  def logPrefix(logType: String): String = {
    val year = config.calender.get(Calendar.YEAR)
    val month = config.calender.get(Calendar.MONTH) + 1
    val day = config.calender.get(Calendar.DAY_OF_MONTH)
    val names = config.parties.sorted.mkString("_").replaceAll("""\s""", "_")
    val park = config.selectedPark.name.replaceAll("""\s""", "_")
    s"${logRoot}/${config.date.month}_${config.date.day}" +
      s"/${park}/${names}/${logType}/${year}_${month}_${day}/${System.currentTimeMillis()}"
  }

  lazy val statsLogDir: String = {
    logPrefix("stats")
  }

  val attractionStatsMap = MatchedAttractionStatsMap()

  def writeToFile(file: File, content: String): Unit = {
    logInfo(s"updating $file")
    file.getParentFile.mkdirs()
    val fos = new FileOutputStream(file)
    fos.write(content.getBytes())
    fos.close()
  }

  def logAttractionStats(): Unit = {
    val file = s"${statsLogDir}/attractions.json"
    val json =
    JsonOperation.objectMapper.writerWithDefaultPrettyPrinter()
      .writeValueAsString(attractionStatsMap.stats)
    writeToFile(new File(file), json)

  }

  def client: WebDriverClient

  private val _fastPasses = collection.mutable.Set.empty[FastPass]

  def login(): Unit = {
    client.login(config.login.userName, config.login.password)
  }

  def selectGuests(): Unit = {
    client.gotoPartySelection()
    logInfo(s"selecting party guests: ${config.parties}")

    client.selectParty(config.parties: _*).withRetry(maxRetry = 10)
  }

  def selectDatePark(): Unit = {

    logInfo(s"selecting date=${config.date}, park=${config.selectedPark.name}")
    client.selectDatePark(
      config.date.month,
      config.date.day,
      config.selectedPark.name
    ).withRetry(callName = "select date park outer retry loop")
  }

  def startGreedyLoop(): Unit = {
    {
      logInfo("closing driver")
      client.closeDriver()
      login()
      _fastPasses ++= client.collectionCurrentDayFastPass(
        config.date.month, config.date.day, config.parks)
      repeatFromBeginning()
    }.withRetry(maxRetry = Int.MaxValue, callName = "outerLoop")
  }

  def repeatFromBeginning(): Unit = {
    selectGuests()
    selectDatePark()

    Try(
      client.continueWithGuestsAfterParkDateSelection()
        .withRetry(callName = "continue with guest")) match {
      case Success(_) =>
        // should reach select experience page here

        repeatedActionRecursive(None, 0)

      case Failure(f) =>

        if (client.fastPassLimitReachedBeforeSelection()) {
          logInfo(s"fast pass limit reached, pick a random one to modify")
          val modifyingFastPass = modifyRandomFastPass(None)

          repeatedActionRecursive(Some(modifyingFastPass), 0)

        } else {
          throw new Exception(
            s"did not reach expected page after data park selection: ${f.getMessage}.")
        }
    }
  }

  def modifyRandomFastPass(previousModify: Option[FastPass]): FastPass = {
    client.modifyRandomFastPass(
      previousModify,
      config.parks,
      config.date.month,
      config.date.day,
      config.parties
    )
  }

  @tailrec
  final def repeatedActionRecursive(
    modifyingPassPass: Option[FastPass], n: Int
  ): Unit = {
    logInfo(s"modify ${modifyingPassPass}")

    modifyingPassPass match {
      case Some(fastPass) if (n > fastPass.attraction.attraction.modifyCycles.getOrElse(10)) =>
        logInfo(s"reached ${n} tries for modifying, rotate by picking a new one")
        val newModifiedFastPass = modifyRandomFastPass(Some(fastPass))
        repeatedActionRecursive(
          Some(newModifiedFastPass), 0
        )

      case _ =>

        if (n > 0) {
          logInfo(s"cycle=${n}, refresh")
          client.driver.navigate().refresh()
        }
        else {
          // client.selectTimeFilter("Morning")
        }
        client.selectTimeValue("10:00 AM") // pick the highest value time

        val (isSelectionPage, isErrorPage) = client.isSelectionOrError()

        if (isErrorPage) {
          logInfo(s"encountered error page, refresh and repeat the action")
          client.driver.navigate().refresh()
          Thread.sleep(1000)
          repeatedActionRecursive(modifyingPassPass, n)
        }
        else if (isSelectionPage) {
          logInfo(s"reached selection page, pick greedy action")

          pickGreedyAction(modifyingPassPass) match {
            case Some((action, initialEval)) =>
              client.moveAndClick(action.button)
              client.continueWithGuestBeforeConfirm()
              // at confirmation page here, do final verification with up-to-date information
              val add = action.add
              val remove = client.parseCancellingItems(config.parks)
              logInfo(s"cancelling ${remove.size} items")
              val eval = evaluateFastPassChanges(
                add,
                remove,
                config.hourPreferences.map(
                  prefs => prefs.map(pref => pref.hour -> pref.value).toMap
                ).getOrElse(Map(1d -> 1d))
              )

              if (eval.value != initialEval.value) {
                logWarning(
                  s"initialEval=${initialEval.value} and finalEval=${eval.value} inconsistnet:" +
                    s" final=${eval}, initial=${initialEval}")
              }
              if (eval.value > 0) {
                logInfo(s"confirm ${action.add.attraction.attraction.name} after final eval")

                val confirmationDir = logPrefix(s"confirmation/${add.attraction.attraction.name}")
                writeToFile(
                  new File(s"${confirmationDir}/beforeConfirm.html"),
                  client.driver.getPageSource
                )
                client.confirmSelection()
                client.waitForLoader()
                writeToFile(
                  new File(s"${confirmationDir}/afterConfirm.html"),
                  client.driver.getPageSource
                )
                val fastPasses =
                {
                  val collected = client.collectionCurrentDayFastPass(
                    config.date.month,
                    config.date.day,
                    config.parks
                  )

                  if (collected.isEmpty) {
                    throw new Exception(s"no confirmed fast passes found")
                  }
                  collected
                }.withRetry()

                _fastPasses.clear()
                _fastPasses ++= fastPasses

                val fastPassConfirmed =
                fastPasses.exists {
                  fp =>
                    fp.attraction == add.attraction &&
                      fp.time == add.time && {
                      (add.party -- fp.party).isEmpty
                    }
                }
                if(fastPassConfirmed) {
                  // update selected fast pass if successful
                  logInfo(s"fast passes after update: ${_fastPasses}")
                  // check confirmation page
                  // 1. sameDay
                  // 2. modify another fast pass
                  // 3. confirmation failure

                  repeatFromBeginning()
                } else {
                  throw new Exception(s"failed to confirm ${add}")
                }

              } else {
                logWarning(s"stopping confirming since final eval=${eval.value}")
                // update selected fast pass from the to be cancelled list
                _fastPasses ++= remove
                logInfo(s"fast passes after update: ${_fastPasses}")
                client.driver.navigate().back()

                repeatedActionRecursive(
                  modifyingPassPass, n + 1
                )
              }
            case None =>
              // no better actions, decide what to do
              // 1. if selecting new fast pass, repeat the loop
              // refresh if page has error
              // 2. if modifying existing fast pass,
              // repeat or change the modifying fast pass then repeat
              repeatedActionRecursive(
                modifyingPassPass, n + 1
              )

          }
        }
        else {
          throw new FatalError(s"unrecognized page")
        }
    }

  }

  def verifyFastPass(
    month: String,
    day: Int,
    parks: Seq[Config.Park],
    fastPass: FastPass
  ): Boolean = {

    {
      val fastPasses = client.collectionCurrentDayFastPass(month, day, parks)

      if (fastPasses.isEmpty) {
        throw new Exception(s"no confirmed fast passes found")
      }

      fastPasses.exists {
        fp =>
          fp.attraction == fastPass.attraction &&
            fp.time == fastPass.time && {
            (fastPass.party -- fp.party).isEmpty
          }
      }
    }.withRetry()
  }

  def pickGreedyAction(
    modify: Option[FastPass]
  ): Option[(FastPassChanges, EvaluationWithMetadata)] = {

    val availableFastPasses: Seq[(AttractionWithParkLand, Seq[(FastPass, WebElement)])] =
      client.getAvailableFastPassSelection(
        config.parks,
        config.parties
      )

    availableFastPasses.foreach {
      case (attraction, fastPasses) =>
        attractionStatsMap.addAttraction(
          attraction,
          fastPasses.map(_._1.time)
        )
    }

    logAttractionStats()

    logInfo(s"${availableFastPasses.size} available fast passes")
    if (availableFastPasses.isEmpty) {
      None
    } else {
      val changesWithEvaluation: Seq[(FastPassChanges, EvaluationWithMetadata)] =
        availableFastPasses.flatMap(_._2).map {
          case (fastPass, button) =>

            val conflicted = _fastPasses.filter(_.conflictWith(fastPass))
            val change = FastPassChanges(fastPass, conflicted ++ modify, button)
            val eval = evaluateFastPassChanges(
              change.add,
              change.remove,
              config.hourPreferences.map(
                prefs => prefs.map(pref => pref.hour -> pref.value).toMap
              ).getOrElse(Map(1d -> 1d))
            )
            logDebug(s"change: ${change}")
            logDebug(s"evaluation: ${eval}")
            (change, eval)
        }

      val bestChange = changesWithEvaluation.maxBy(_._2.value)
      logInfo(s"best change value=${bestChange._2.value}: ${bestChange}")
      if (bestChange._2.value > 0) {
        Some(bestChange)
      } else {
        None
      }
    }

  }

  def evaluateFastPass(
    fastPass: FastPass,
    defaultHourPref: Map[Double, Double]
  ): EvaluationWithMetadata = {
    val baseValue = fastPass.attraction.attraction.value

    val hourPref =
      fastPass.attraction.attraction
        .hourPreferences.map(
        prefs => prefs.map(pref => pref.hour -> pref.value).toMap
      ).getOrElse(defaultHourPref)

    val time = fastPass.time
    val inputHour = time.hour + time.min * 1d / 60

    val timeAdjustment = FastPassGreedyBooker.getInterpolatedValue(
      hourPref, inputHour
    )

    EvaluationWithMetadata(
      baseValue * timeAdjustment,
      Map(
        "baseValue" -> baseValue,
        "hourPref" -> hourPref.toSeq.sortBy(_._1),
        "inputHour" -> inputHour,
        "timeAdjustment" -> timeAdjustment
      )
    )

  }

  def evaluateFastPassChanges(
    add: FastPass,
    remove: Iterable[FastPass],
    defaultHourPreferences: Map[Double, Double]
  ): EvaluationWithMetadata = {

    val addEvaluation =
      evaluateFastPass(
        add,
        defaultHourPreferences
      )

    val removeEvaluations = remove.map(
      evaluateFastPass(_, defaultHourPreferences)
    )

    val maxRemove: Option[EvaluationWithMetadata] =
      if (removeEvaluations.isEmpty) {
        None
      } else {
        Some(removeEvaluations.toSeq.maxBy(_.value))
      }


    val finalValue = addEvaluation.value - maxRemove.map(_.value).getOrElse(0d)

    EvaluationWithMetadata(
      finalValue,
      Map(
        "add" -> addEvaluation,
        "maxRemove" -> maxRemove,
        "remove" -> removeEvaluations
      )
    )
  }
}

object FastPassGreedyBooker {

  case class AttractionStats(
    attractionName: String,
    var totalAppearance: Int = 0,
    stats: collection.mutable.Map[HourAndMinute, ExperienceStats] = collection.mutable.Map.empty
  ) {
    def addAppearance(hourAndMinutes: Seq[HourAndMinute]): AttractionStats = {
      totalAppearance += 1

      val time = System.currentTimeMillis()
      hourAndMinutes.foreach {
        hourAndMinute =>
          val updatedStats =
            stats.get(
              hourAndMinute
            ).map(_.addAppearance()).getOrElse(ExperienceStats(time, time, 1))
          stats += (hourAndMinute -> updatedStats)
      }
      this
    }
  }

  case class ExperienceStats(
    firstAppearTime: Long,
    lastAppearTime: Long,
    totalAppear: Int
  ) {
    def addAppearance(): ExperienceStats = {
      this.copy(
        lastAppearTime = System.currentTimeMillis(),
        totalAppear = totalAppear + 1
      )
    }
  }

  case class MatchedAttractionStatsMap(
    stats: collection.mutable.Map[String, AttractionStatsMap] = collection.mutable.Map.empty
  ) {
    def addAttraction(
      attraction: AttractionWithParkLand,
      hourAndMinute: Seq[HourAndMinute]): MatchedAttractionStatsMap = {

      val updatedStats: AttractionStatsMap =
        stats.getOrElse(
          attraction.attraction.name,
          AttractionStatsMap(attraction.attraction.value)
        ).addAttraction(attraction.rawName, hourAndMinute)

      stats += (attraction.attraction.name -> updatedStats)
      this
    }
  }

  case class AttractionStatsMap(
    baseValue: Double,
    stats: collection.mutable.Map[String, AttractionStats] = collection.mutable.Map.empty
  ) {
    def addAttraction(
      name: String,
      hourAndMinute: Seq[HourAndMinute]): AttractionStatsMap = {

      val updatedStats = stats.getOrElse(name, AttractionStats(name))
        .addAppearance(hourAndMinute)

      stats += (name -> updatedStats)
      this
    }
  }

  case class EvaluationWithMetadata(value: Double,
    metadata: Map[String, Any]
  )

  def getInterpolatedValue(function: Map[Double, Double],
    input: Double
  ): Double = {
    if (function.isEmpty) {
      throw FatalError(s"function cannot be empty")
    }
    val sortedValues = function.toSeq.sortBy(_._1)
    val firstIndex = (0 until sortedValues.size - 1).collectFirst {
      case i if sortedValues(i)._1 <= input && input <= sortedValues(i + 1)._1 => i
    }

    firstIndex match {
      case None if input < sortedValues.headOption.get._2 =>
        sortedValues.headOption.get._2
      case None if input > sortedValues.lastOption.get._2 =>
        sortedValues.lastOption.get._2
      case Some(index) =>
        val (lowerX, lowerY) = sortedValues(index)
        val (upperX, upperY) = sortedValues(index + 1)
        // linear interpolation
        lowerY + (upperY - lowerY) * (input - lowerX) / (upperX - lowerX)
      case _ =>
        throw internal.FatalError(s"input=$input does not match a range correctly: " +
          s"${sortedValues.map(_._1)}")
    }
  }

  case class FastPassChanges(
    add: FastPass, // always add 1 at a time
    remove: Iterable[FastPass], // can remove multiple ones
    button: WebElement
  ) {
    override def toString: String =
      s"add ${add.attraction.attraction.name} at ${add.time}, " +
        s"remove ${remove.map(_.attraction.attraction.name)}: ${add}, remove=${remove}"
  }

}
