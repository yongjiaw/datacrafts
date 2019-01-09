package org.datacrafts.app.dwfpp

import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver

import org.datacrafts.dwfpp.Config
import org.datacrafts.logging.Slf4jLogging

object ChromeClient extends WebDriverClient with Slf4jLogging.Default {
  override def createDriver(): WebDriver = {
    new ChromeDriver()
  }

  def main(args: Array[String]): Unit = {
    System.setProperty("webdriver.chrome.driver", "/Users/ywang/Downloads/chromedriver_3")
    val config = Config.parse("dwfpp/disneyworld_fastpassplus.conf")
  }
}
