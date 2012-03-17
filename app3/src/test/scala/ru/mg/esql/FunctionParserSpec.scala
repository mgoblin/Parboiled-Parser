package ru.mg.esql

import org.specs.SpecificationWithJUnit
import statements.FunctionParser
import org.parboiled.scala.parserunners.ReportingParseRunner


class FunctionParserSpec extends SpecificationWithJUnit {

  val parser = new FunctionParser { override val buildParseTree = true }

  "Function parser header rule " should {
    "Parse procedure header declaration" in  {

      val input = "create procedure"
      val result = ReportingParseRunner(parser.FunctionHeader).run(input)
      
      val decl = result.resultValue
      decl must_== input

    }

    "Parse function header declaration" in  {

      val input = "create function"
      val result = ReportingParseRunner(parser.FunctionHeader).run(input)

      val decl = result.resultValue
      decl must_== input

    }
  }

  "Function parser signature rule " should {
    "Parse procedure/function signature" in  {

      val input = "Main()"
      val result = ReportingParseRunner(parser.FunctionSignature).run(input)

      val signature = result.resultValue
      signature must_== "Main"
    }
  }

  "Function parser" should {
    "parse procedure internal declaration without body" in {

      val input = "CREATE PROCEDURE Main();"
      val result = ReportingParseRunner(parser.FunctionStatement).run(input)

      val decl = result.resultValue
      decl.text must_== "Main"

    }

    "parse procedure internal declaration with body and no params" in {

      val input = """CREATE PROCEDURE Main() -- This is cool
        call;
        ;"""
      val result = ReportingParseRunner(parser.FunctionStatement).run(input)

      val decl = result.resultValue
      decl.text must_== "Main"
      decl.statements.size must_== 1
      decl.statements(0).isInstanceOf[LineStatementNode] must_== true
    }
  }

}
