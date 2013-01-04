package ru.mg.parser.esql

import io.Source
import org.parboiled.scala._
import ru.mg.parsing.esql.parser.parts.ModuleParser
import ru.mg.parsing.esql.ast.{LineCommentNode, BlockCommentNode, LineStatementNode, FunctionNode}
import org.specs2.mutable.SpecificationWithJUnit


class ModuleParserSpec extends SpecificationWithJUnit {

  val parser = new ModuleParser {
    override val buildParseTree = true
  }

  "Module parser" should {
    "parser empty module decl" in {
      val input = Source.fromURL(getClass.getResource("/esql/emptyModule.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.linesRange must_== (1 to 3)
      result.resultValue.statements.length must_== 0
    }

    "parse module with functions" in {
      val input = Source.fromURL(getClass.getResource("/esql/module1.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.linesRange must_== (1 to 67)
      result.resultValue.statements.length must_== 3
      result.resultValue.statements(0).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(0).parent.get must_== result.resultValue
      result.resultValue.statements(1).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(1).parent.get must_== result.resultValue
      result.resultValue.statements(2).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(2).parent.get must_== result.resultValue
    }

    "parse module with comment and function" in {
      val input = Source.fromURL(getClass.getResource("/esql/module2.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.linesRange must_== (1 to 187)
      result.resultValue.statements.length must_== 5
      result.resultValue.statements(0).isInstanceOf[BlockCommentNode] must_== true
      result.resultValue.statements(1).isInstanceOf[LineCommentNode] must_== true
      result.resultValue.statements(2).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(3).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(3).isInstanceOf[FunctionNode] must_== true
    }

    "parse module with global vars and functions" in {
      val input = Source.fromURL(getClass.getResource("/esql/module3.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.linesRange must_== (1 to 189)
      result.resultValue.statements.length must_== 6
      result.resultValue.statements(0).isInstanceOf[BlockCommentNode] must_== true
      result.resultValue.statements(1).isInstanceOf[LineStatementNode] must_== true
      result.resultValue.statements(2).isInstanceOf[LineCommentNode] must_== true
      result.resultValue.statements(3).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(4).isInstanceOf[FunctionNode] must_== true
      result.resultValue.statements(5).isInstanceOf[FunctionNode] must_== true
    }
  }

}

