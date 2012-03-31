package ru.mg.esql

import ast.FunctionNode
import org.specs.SpecificationWithJUnit
import statements.ModuleParser
import io.Source
import org.parboiled.scala.parserunners.{TracingParseRunner, ReportingParseRunner}


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
      val result = TracingParseRunner(parser.ModuleStatement).run(input)

      result.hasErrors must_== false
      result.resultValue.startLine must_== 1
      result.resultValue.statements.length must_== 1
      result.resultValue.statements(0).isInstanceOf[FunctionNode] must_== true
    }

    "parse module with comment and function" in {
      fail("not implemented yet")
    }

    "parse module with global vars and functions" in {
      fail("not implemented yet")
    }
  }

}

