package ru.mg.esql

import org.specs.SpecificationWithJUnit
import statements.CompoundStatementParser
import org.parboiled.scala.parserunners.ReportingParseRunner


class CompoundStatementSpec extends SpecificationWithJUnit {

  val parser = new CompoundStatementParser { override val buildParseTree = true }

  "Compound statement parser" should {
    "parse begin end statement" in {

      val input = "BEGIN  END;"
      val result = ReportingParseRunner(parser.BeginEnd).run(input).resultValue

      result mustNotBe null
      result.text must_== "BEGIN END"
      result.startLine must_== 1
      result.firstLineComment must_== None
      result.lastLineComment must_== None
    }
  }
}
