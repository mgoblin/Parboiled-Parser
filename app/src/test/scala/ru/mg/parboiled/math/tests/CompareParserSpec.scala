package ru.mg.parboiled.math.tests

import org.specs._
import ru.mg.parboiled.math.CompareParser
import org.parboiled.scala._
import org.parboiled.support.ParseTreeUtils


class CompareParserSpec extends SpecificationWithJUnit {

  val parser = new CompareParser() { override val buildParseTree = true }

  def check(result: ParsingResult[Nothing]) {
    val treeRoot = result.parseTreeRoot

    result must not be null
    result.parseErrors must be empty;
    treeRoot.getChildren.size must_== 2
    treeRoot.getLabel mustEq "InputLine"
    treeRoot.getChildren.get(0).getLabel mustEq "Expression"
    treeRoot.getChildren.get(1).getLabel mustEq "EOI"
  }

  "Compare parser" should {
    "parse > expression" in {
      val input = "42 > 40"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
    }

    "parse < expression " in {
      val input = "42 < 40"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
    }

    "parse > with complex math expression" in {
      val input = "10 + 2*5 < 3 * (40/10 - 1)"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
    }

    "parse with complex compare expression" in {
      val input = "10 < 3 < 1"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
      println(ParseTreeUtils.printNodeTree(result))
    }

  }
}