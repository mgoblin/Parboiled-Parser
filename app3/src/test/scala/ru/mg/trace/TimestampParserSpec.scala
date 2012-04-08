package ru.mg.trace

import org.specs.SpecificationWithJUnit
import org.parboiled.scala.parserunners.ReportingParseRunner


class TimestampParserSpec extends SpecificationWithJUnit {

  val parser = new TimestampParser { override val buildParseTree = true }

  "Timestamp parser" should {

    "parse timestamp yyyy-mm-dd hh:mm:ss" in {

      val input = "2008-04-23 14:22:25.481815"

      val run = ReportingParseRunner(parser.Timestamp).run(input)
      val result = run.resultValue

      result mustNotBe null
      result must_== input
    }

    "not parse invalid data" in {

      val input = "2008-04-23 14:2:25.481815"

      val run = ReportingParseRunner(parser.Timestamp).run(input)
      run.hasErrors must_== true
    }
  }

}
