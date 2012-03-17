package ru.mg.esql

import org.specs.SpecificationWithJUnit
import org.parboiled.scala.parserunners.ReportingParseRunner
import org.parboiled.scala.ParsingResult
import statements.StatementParser


class StatementParserSpec extends SpecificationWithJUnit {

  val parser = new StatementParser { override val buildParseTree = true }

  def check(result: ParsingResult[AstNode], text: String): Any = {
    result.hasErrors mustBe false
    result.parseTreeRoot mustNotBe null
    result.parseTreeRoot.getChildren mustNotBe empty

    val commentNode = result.resultValue
    commentNode.text must_== text
    commentNode.line must_== 1
  }

  "Statement parser" should {

    "Parse line comment" in  {

      val input = "-- This is a comment"
      val result = ReportingParseRunner(parser.LineComment).run(input)

      check(result, "This is a comment")
    }

    "Dont be greedy on line comments" in {

      val input = "-- \nThis is a comment"
      val result = ReportingParseRunner(parser.LineComment).run(input)

      val value = result.resultValue
      value.text must_== ""
    }

    "Parse line executable statement" in  {

      val input = "declare CacheQueueTable SHARED ROW;"
      val result = ReportingParseRunner(parser.LineStatement).run(input)

      check(result, "declare CacheQueueTable SHARED ROW")
    }

    "Parse multi line executable statement" in  {

      val input = "declare \n  CacheQueueTable \nSHARED ROW;"
      val result = ReportingParseRunner(parser.LineStatement).run(input).resultValue

      result.text must_== "declare \n  CacheQueueTable \nSHARED ROW"
      result.line must_== 3
    }

    "Parse executable statement with line comment" in  {

      val input = "declare CacheQueueTable SHARED ROW; -- comment"
      val result = ReportingParseRunner(parser.LineStatement).run(input)

      check(result, "declare CacheQueueTable SHARED ROW")
      result.resultValue.comment.get.text must_== "comment"
    }

    "Dont be greedy with line comments on next line" in {

      val input = "declare CacheQueueTable SHARED ROW; \n-- comment"
      val result = ReportingParseRunner(parser.LineStatement).run(input).resultValue

      result.text must_== "declare CacheQueueTable SHARED ROW"

    }
  }

}
