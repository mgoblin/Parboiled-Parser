package ru.mg.parboiled.math.tests

import org.specs._
import ru.mg.parboiled.math.MathParser
import org.parboiled.scala.parserunners.ReportingParseRunner
import org.parboiled.scala.ParsingResult

class MathParserSpec extends SpecificationWithJUnit {
  val parser = new MathParser {
    override val buildParseTree = true
  }

  def check(result: ParsingResult[Nothing]) {
    val treeRoot = result.parseTreeRoot

    result must not be null
    result.parseErrors must be empty;
    treeRoot.getChildren.size must_== 2
    treeRoot.getLabel mustEq "InputLine"
    treeRoot.getChildren.get(0).getLabel mustEq "Expression"
    treeRoot.getChildren.get(1).getLabel mustEq "EOI"
  }

  "Math parser" should {

    "parse math expressions with addition and substraction" in {
      val input = "1+2"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
    }

    "parse math expressions with multiplication and division" in {
      val input = "1*2"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
    }

    "parse mixed math expressions with + - * /" in {
      val input = "1*2+3/2"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
    }

    "parse mixed math expressions with + - * / and parens" in {
      val input = "(2+(1+20)*5/10+9)"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      check(result)
    }

  }
}