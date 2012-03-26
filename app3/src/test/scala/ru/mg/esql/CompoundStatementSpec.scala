package ru.mg.esql

import org.specs.SpecificationWithJUnit
import statements.CompoundStatementParser
import org.parboiled.scala.parserunners.ReportingParseRunner


class CompoundStatementSpec extends SpecificationWithJUnit {

  val parser = new CompoundStatementParser { override val buildParseTree = true }

  "Compound statement parser" should {
    "parse begin end statement" in {

      val input = "BEGIN  END ;"
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "BEGIN END"
      result.startLine must_== 1
      result.statements.length must_==  0
    }

    "parse begin end statement with first line comment" in {

      val input = """
        BEGIN -- start begin
        END;
      """
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "BEGIN END"
      result.startLine must_== 1
      result.statements.length must_==  1
    }

    "parse begin end statement with first and last line comment" in {

      val input = """
        BEGIN -- start begin
        END; -- end comment
      """
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "BEGIN END"
      result.startLine must_== 1
      result.statements.length must_==  1
    }

    "parse begin end statement with last line comment" in {

      val input = """
        BEGIN -- start begin
          call;
        END; -- end comment
      """
      val run = ReportingParseRunner(parser.BeginEndStatement).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.text must_== "BEGIN END"
      result.startLine must_== 1
      result.statements.length must_== 2
      result.statements(0).text must_== "start begin"
    }
  }
}