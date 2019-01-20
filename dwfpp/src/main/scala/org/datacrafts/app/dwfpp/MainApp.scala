package org.datacrafts.app.dwfpp

object MainApp {
  def main(args: Array[String]): Unit = {
    System.setProperty("webdriver.chrome.driver", "./chromedriver")
    val config = BookerConfig.parse("./application.conf")
    val booker = new ChromeGreedyBooker(config)
    booker.startGreedyLoop()
  }
}
