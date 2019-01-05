package org.datacrafts.dwfpp
import scala.util.{Failure, Success}

import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver

object ChromeFastPassBooker extends FastPassBooker {

  override lazy val driver: WebDriver = {
    System.setProperty("webdriver.chrome.driver", config.chromeDriverLocation)
    new ChromeDriver()
  }

  override def config: FastPassBookerConfig = {
    _config.getOrElse(
      throw new Exception(s"config not initialized yet")
    )
  }

  private var _config: Option[FastPassBookerConfig] = None

  def main(args: Array[String]): Unit = {
    val configParser = new ConfigParser[FastPassBookerConfig]
    _config = Some(configParser.parseConfig("dwfpp/application.conf"))

    driver.get(config.fastPassConfig.initialPageUrl)

    blockUntilSelectionPage()

    selectExperience()
  }
}
