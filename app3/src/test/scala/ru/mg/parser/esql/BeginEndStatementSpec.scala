package ru.mg.parser.esql

import org.specs2.mutable.SpecificationWithJUnit
import io.Source
import org.parboiled.scala.parserunners.ReportingParseRunner
import ru.mg.parsing.esql.parser.parts.StatementParser


class BeginEndStatementSpec extends SpecificationWithJUnit {

  val parser = new StatementParser { override val buildParseTree = true }

  "Compound statement parser" should {
    "parse begin end statement" in {

      val input = "BEGIN  \nEND ;"
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result must_!=  null
      result.text must_== ""
      result.linesRange must_== (1 to 1)
    }

    "parse begin end statement with first line comment" in {

      val input = "BEGIN -- start begin\n END;"
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result must_!= null
      result.text must_== "-- start begin"
      result.linesRange must_== (1 to 1)
    }

    "parse begin end statement with first and last line comment" in {

      val input = """BEGIN -- start begin
      |END; -- end comment
      """.stripMargin

      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result must_!= null
      result.text must_== "-- start begin"
      result.linesRange must_== (1 to 1)
    }

    "parse begin end statement with last line comment" in {

      val input = """BEGIN -- start begin
      |    call;
      |END; -- end comment
      """.stripMargin
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result must_!= null
      result.text must_== "-- start begin\n    call;"
      result.linesRange must_== (1 to 2)
    }

    "parse begin.esql" in {
      val input = Source.fromURL(getClass.getResource("/esql/begin.esql")).getLines().mkString("\n")
      val out = "-- beginning of atomic block. Processing is single threaded until the END; is reached"

      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue
      result must_!= null
      result.text must_== out
    }
  }
}
