package ru.mg.trace

import org.parboiled.scala._


trait TimestampParser extends Parser {

  def Timestamp: Rule1[String] = rule {
    Date ~ Space ~ Time ~ dot ~ Millis ~~> { join }
  }

  def Date: Rule1[String] = rule {
    Year ~ DateSeparator ~ Month ~ DateSeparator ~ Day ~~> { join }
  }

  def Year = FourDigits
  def Month = TwoDigits
  def Day = TwoDigits


  def Time = rule {
    hh ~ TimeSeparator ~ mm ~ TimeSeparator ~ ss ~~> { join }
  }

  def hh = TwoDigits
  def mm = TwoDigits
  def ss = TwoDigits

  def Millis = SixDigits

  def TwoDigits = rule { nTimes(2, "0" - "9") ~> { _.toString } }
  def FourDigits = rule { nTimes(4, "0" - "9") ~> { _.toString } }
  def SixDigits = rule { nTimes(6, "0" - "9") ~> { _.toString } }

  def DateSeparator = str("-") ~> { _.toString }
  def TimeSeparator = str(":") ~> { _.toString }
  def Space = str(" ") ~> { _.toString }
  def dot = str(".") ~> { _.toString }

  def join = {
    (s1: String, s2: String, s3: String, s4: String, s5: String) => List(s1, s2, s3, s4, s5).mkString
  }

}
