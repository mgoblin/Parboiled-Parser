package ru.mg.esql

import ast.{LineStatementNode, CommentNode, FunctionNode}
import org.specs.SpecificationWithJUnit
import statements.ModuleParser
import io.Source
import org.parboiled.scala._


class ModuleParserSpec extends SpecificationWithJUnit {

  val parser = new ModuleParser {
    override val buildParseTree = true
  }

  "Module parser" should {
    "parser empty module decl" in {
      val input = Source.fromURL(getClass.getResource("/emptyModule.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.startLine must_== 1
      result.resultValue.statements.length must_== 0
    }

    "parse module with functions" in {
      val input = Source.fromURL(getClass.getResource("/module1.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.startLine must_== 1
      result.resultValue.statements.length must_== 3
      result.resultValue.statements(0).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(1).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(2).isInstanceOf[FunctionNode] must_== true
    }

    "parse module with comment and function" in {
      val input = Source.fromURL(getClass.getResource("/module2.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.startLine must_== 1
      result.resultValue.statements.length must_== 5
      result.resultValue.statements(0).isInstanceOf[CommentNode] must_== true
      result.resultValue.statements(1).isInstanceOf[CommentNode] must_== true
      result.resultValue.statements(2).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(3).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(3).isInstanceOf[FunctionNode] must_== true
    }

    "parse module with global vars and functions" in {
      val input = Source.fromURL(getClass.getResource("/module3.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.startLine must_== 1
      result.resultValue.statements.length must_== 6
      result.resultValue.statements(0).isInstanceOf[CommentNode] must_== true
      result.resultValue.statements(1).isInstanceOf[LineStatementNode] must_== true
      result.resultValue.statements(2).isInstanceOf[CommentNode] must_== true
      result.resultValue.statements(3).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(4).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(5).isInstanceOf[FunctionNode] must_== true
    }
  }

}

