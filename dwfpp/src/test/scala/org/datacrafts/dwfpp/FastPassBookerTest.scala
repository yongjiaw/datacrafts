package org.datacrafts.dwfpp

object FastPassBookerTest {
  def main(args: Array[String]): Unit = {
    ChromeFastPassBooker.main(Array("dwfpp/application.conf"))
  }
}
