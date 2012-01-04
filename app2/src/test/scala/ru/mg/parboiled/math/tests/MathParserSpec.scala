package ru.mg.parboiled.math.tests

import org.specs._
import ru.mg.parboiled.math.MathParser
import org.parboiled.scala.parserunners.ReportingParseRunner
import org.parboiled.scala.ParsingResult
import org.parboiled.errors.ErrorUtils

class MathParserSpec extends SpecificationWithJUnit {
  val parser = new MathParser {
    override val buildParseTree = true
  }

  def checkTree(parsingResult: ParsingResult[Int]) {
    val treeRoot = parsingResult.parseTreeRoot

    parsingResult must not be null
    parsingResult.parseErrors must be empty;
    treeRoot.getChildren.size must_== 2
    treeRoot.getLabel mustEq "InputLine"
    treeRoot.getChildren.get(0).getLabel mustEq "Expression"
    treeRoot.getChildren.get(1).getLabel mustEq "EOI"

  }

  def checkEquals(parsingResult: ParsingResult[Int], expected: Int) {
    parsingResult.result match {
      case Some(value) => value must_== expected
      case None => fail(ErrorUtils.printParseErrors(parsingResult))
    }
  }



  "Math parser" should {

    "parse math expressions with addition and substraction" in {
      val input = "1+2"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      checkEquals(result, 3)
      checkTree(result)
    }

    "parse math expressions with multiplication and division" in {
      val input = "2/1"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      checkEquals(result, 2)
      checkTree(result)
    }

    "parse mixed math expressions with + - * /" in {
      val input = "1*2+6/2"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      checkEquals(result, 5)
      checkTree(result)
    }

    "parse mixed math expressions with + - * / and parens" in {
      val input = "2+(1+19)*2/10+1"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      checkTree(result)
      checkEquals(result, 7)
    }

    "correctly handle whitespaces inside expression" in {
      val input = "10 * 2 - ( 12 + 4 ) / 2"
      val result = ReportingParseRunner(parser.InputLine).run(input)
      checkTree(result)
      checkEquals(result, 12)
    }

  }
}