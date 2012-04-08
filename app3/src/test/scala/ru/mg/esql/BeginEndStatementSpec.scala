package ru.mg.esql

import org.specs.SpecificationWithJUnit
import io.Source
import org.parboiled.scala.parserunners.ReportingParseRunner


class BeginEndStatementSpec extends SpecificationWithJUnit {

  val parser = new StatementParser { override val buildParseTree = true }

  "Compound statement parser" should {
    "parse begin end statement" in {

      val input = "BEGIN  \nEND ;"
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== ""
      result.startLine must_== 1
    }

    "parse begin end statement with first line comment" in {

      val input = "BEGIN -- start begin\n END;"
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "-- start begin"
      result.startLine must_== 1
    }

    "parse begin end statement with first and last line comment" in {

      val input = """BEGIN -- start begin
      |END; -- end comment
      """.stripMargin

      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "-- start begin"
      result.startLine must_== 1
    }

    "parse begin end statement with last line comment" in {

      val input = """BEGIN -- start begin
      |    call;
      |END; -- end comment
      """.stripMargin
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "-- start begin\n    call;"
      result.startLine must_== 1
    }

    "parse begin.esql" in {
      val input = Source.fromURL(getClass.getResource("/begin.esql")).getLines().mkString("\n")
      val out = "-- beginning of atomic block. Processing is single threaded until the END; is reached"

      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue
      result mustNotBe null
      result.text must_== out
    }
  }
}
