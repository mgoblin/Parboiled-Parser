package ru.mg.esql

import ast.EsqlAstNode
import org.specs.SpecificationWithJUnit
import org.parboiled.scala.ParsingResult
import org.parboiled.scala.parserunners.ReportingParseRunner


class StatementParserSpec extends SpecificationWithJUnit {

  val parser = new StatementParser { override val buildParseTree = true }

  def check(result: ParsingResult[EsqlAstNode], text: String): Any = {
    result.hasErrors mustBe false
    result.parseTreeRoot mustNotBe null
    result.parseTreeRoot.getChildren mustNotBe empty

    val node = result.resultValue
    node.text must_== text
    node.startLine must_== 1
  }

  "Statement parser" should {

    "Parse line comment" in  {

      val input = "-- This is a comment\n"
      val result = ReportingParseRunner(parser.LineComment).run(input)

      check(result, "This is a comment")
    }

    "Dont be greedy on line comments" in {

      val input = "-- \nThis is a comment"
      val result = ReportingParseRunner(parser.LineComment).run(input)

      val value = result.resultValue
      value.text must_== ""
    }

    "parse line comments with code elements" in {

      val input = "-- This is a comment END;\n"
      val result = ReportingParseRunner(parser.LineComment).run(input)

      result.hasErrors must_== false
      val value = result.resultValue
      value.text must_== "This is a comment END;"
    }

    "Parse line statement" in  {

      val input = "declare CacheQueueTable SHARED ROW;"
      val result = ReportingParseRunner(parser.DeclareStatement).run(input)

      check(result, "CacheQueueTable SHARED ROW")
    }

    "Parse multi line statement" in  {

      val input = "declare \n  CacheQueueTable \nSHARED ROW;"
      val result = ReportingParseRunner(parser.DeclareStatement).run(input).resultValue

      result.text must_== "CacheQueueTable \nSHARED ROW"
      result.startLine must_== 1
    }

    "Parse line statement with line comment" in  {

      val input = "declare CacheQueueTable SHARED ROW; -- comment"
      val result = ReportingParseRunner(parser.DeclareStatement).run(input)

      check(result, "CacheQueueTable SHARED ROW")
    }

    "Parse line statement with block comment" in  {

      val input =
        """declare CacheQueueTable SHARED ROW;
        /**
          comment
        **/
        """
      val result = ReportingParseRunner(parser.DeclareStatement).run(input)

      val node = result.resultValue
      node.text must_== "CacheQueueTable SHARED ROW"
      node.startLine must_== 1
    }

    "Dont be greedy with line comments on next line" in {

      val input = "declare CacheQueueTable SHARED ROW; \n-- comment"
      val result = ReportingParseRunner(parser.DeclareStatement).run(input).resultValue

      result.text must_== "CacheQueueTable SHARED ROW"
    }

    "Dont be greedy with statements" in {

      val input = "declare CacheQueueTable SHARED ROW; \n call;"
      val result = ReportingParseRunner(parser.DeclareStatement).run(input).resultValue

      result.text must_== "CacheQueueTable SHARED ROW"

    }

    "Parse empty block comment" in {

      val input = "/*\n*/"
      val result = ReportingParseRunner(parser.BlockComment).run(input)

      result.resultValue.text must_== ""
    }

    "Parse block comment (one line)" in {

      val input = "/* comment */"
      val result = ReportingParseRunner(parser.BlockComment).run(input)

      result.resultValue.text must_== "comment"
    }

    "Parse block comment" in {

      val input =
        """/*
        *  comment
        */
        """
      val result = ReportingParseRunner(parser.BlockComment).run(input)

      result.resultValue.text must_== "*  comment"
    }
  }



}
