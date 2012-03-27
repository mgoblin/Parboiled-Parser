package ru.mg.esql

import org.specs.SpecificationWithJUnit
import org.parboiled.scala.parserunners.ReportingParseRunner
import statements.StatementParser


class BeginEndStatementSpec extends SpecificationWithJUnit {

  val parser = new StatementParser { override val buildParseTree = true }

  "Compound statement parser" should {
    "parse begin end statement" in {

      val input = "BEGIN  END ;"
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "  "
      result.startLine must_== 1
    }

    "parse begin end statement with first line comment" in {

      val input = "BEGIN -- start begin END;"
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== " -- start begin "
      result.startLine must_== 1
    }

    "parse begin end statement with first and last line comment" in {

      val input = """BEGIN -- start begin
        END; -- end comment
      """
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== " -- start begin\n        "
      result.startLine must_== 1
    }

    "parse begin end statement with last line comment" in {

      val input = """BEGIN -- start begin
          call;
        END; -- end comment
      """
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== " -- start begin\n          call;\n        "
      result.startLine must_== 1
    }
  }
}
