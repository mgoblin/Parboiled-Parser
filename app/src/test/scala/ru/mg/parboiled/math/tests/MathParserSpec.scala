package ru.mg.parboiled.math.tests

import org.specs._
import ru.mg.parboiled.math.MathParser
import org.parboiled.scala.parserunners.ReportingParseRunner

class MathParserSpec extends SpecificationWithJUnit {

    "Parser" should {

      val parser = new MathParser {override val buildParseTree = true }

      "parse math expressions" in {
        val input = "1+2"
        val result = ReportingParseRunner(parser.Expression).run(input)
        val treeRoot = result.parseTreeRoot

        result must not be null
        result.parseErrors must be empty;
        treeRoot.getChildren.size must_== 2
        treeRoot.getLabel mustEq "Expression"
        treeRoot.getChildren.get(0).getLabel mustEq "Term"
        treeRoot.getChildren.get(1).getLabel mustEq "ZeroOrMore"
      }

      "correctly handle whitespaces inside expression" in {
        val input = "1 + 2"
        val result = ReportingParseRunner(parser.Expression).run(input)
        val treeRoot = result.parseTreeRoot

        result must not be null
        result.parseErrors must be empty;
        treeRoot.getChildren.size must_== 2
        treeRoot.getLabel mustEq "Expression"
        treeRoot.getChildren.get(0).getLabel mustEq "Term"
        treeRoot.getChildren.get(1).getLabel mustEq "ZeroOrMore"
      }
    }
}