package org.datacrafts.app.dwfpp

import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver

import org.datacrafts.logging.Slf4jLogging

class ChromeGreedyBooker(config: BookerConfig) extends FastPassGreedyBooker(config) {
  override lazy val client: WebDriverClient =
    new WebDriverClient with Slf4jLogging.Default {
      override def createDriver(): WebDriver = new ChromeDriver()
    }
}
