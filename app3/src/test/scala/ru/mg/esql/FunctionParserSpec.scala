package ru.mg.esql

import ast.BeginEndNode
import org.specs.SpecificationWithJUnit
import statements.FunctionParser
import scala.io.Source
import org.parboiled.scala.parserunners.ReportingParseRunner


class FunctionParserSpec extends SpecificationWithJUnit {

  val parser = new FunctionParser { override val buildParseTree = true }

  "Function parser signature rule " should {
    "parse procedure/function signature" in  {

      val input = "Main()"
      val result = ReportingParseRunner(parser.FunctionSignature).run(input)

      val signature = result.resultValue
      signature must_== input
    }
  }

  "Function parser parameter declaration rule" should {
    "parse parameter declaration with modifier" in {

      val input = "(IN InputExcList REFERENCE)"
      val result = ReportingParseRunner(parser.ParamsDecl).run(input)
      
      val paramDecl = result.resultValue
      paramDecl must_== input

    }

    "parse parameter declaration without modifier" in {

      val input = "(InputExcList REFERENCE)"
      val result = ReportingParseRunner(parser.ParamsDecl).run(input)

      val paramDecl = result.resultValue
      paramDecl must_== input

    }

    "parse parameter declaration with CONSTANT" in {

      val input = "(InputExcList CONSTANT REFERENCE)"
      val result = ReportingParseRunner(parser.ParamsDecl).run(input)

      val paramDecl = result.resultValue
      paramDecl must_== input

    }

    "parse one parameter" in  {
      val input = "(InputExcList CONSTANT REFERENCE)"
      val result = ReportingParseRunner(parser.ParamsDecl).run(input)

      val paramDecl = result.resultValue
      paramDecl must_== input
    }

    "parse parameter list" in  {
      val input = "(InputExcList REFERENCE, i INT, j INT)"
      val result = ReportingParseRunner(parser.ParamsDecl).run(input)

      val paramDecl = result.resultValue
      paramDecl must_== input
    }
  }

  "Function parser language declaration rule " should {

    "parse function language declaration" in {
      val input = "LANGUAGE ESQL"
      val result = ReportingParseRunner(parser.FunctionLanguage).run(input).resultValue

      result must_== " LANGUAGE ESQL"
    }

  }

  "Function parser" should {
    "not parse internal procedure declaration without body and returns" in {

      val input = "CREATE PROCEDURE Main ();"
      val result = ReportingParseRunner(parser.FunctionStatement).run(input)

      result.hasErrors must_== true

    }

    "not parse internal function declaration without body" in {

      val input = "CREATE FUNCTION Main() RETURNS INT;"
      val result = ReportingParseRunner(parser.FunctionStatement).run(input)

      result.hasErrors must_== true
    }

    "parse procedure internal declaration with body and no params" in {

      val input =
        """CREATE PROCEDURE Main() -- This is cool
           BEGIN
           END;
        """
      val result = ReportingParseRunner(parser.FunctionStatement).run(input)

      val decl = result.resultValue
      decl.text must_== "Main()"
      decl.statements.size must_== 2
      decl.statements(1).isInstanceOf[BeginEndNode] must_== true
    }

    "parse function declaration with specified language" in {
      val input =
      """CREATE PROCEDURE Main() LANGUAGE ESQL
         BEGIN
         END;
      """
      val result = ReportingParseRunner(parser.FunctionStatement).run(input)

      val decl = result.resultValue
      decl.text must_== "Main() LANGUAGE ESQL"
      decl.statements.size must_== 1
      decl.statements(0).isInstanceOf[BeginEndNode] must_== true
    }
  }

  "parse function from file function1.esql" in {
    val input = Source.fromURL(getClass.getResource("/function1.esql")).getLines().mkString("\n")
    val result = ReportingParseRunner(parser.FunctionStatement).run(input)

    result.hasErrors must_== false
    result.resultValue.startLine must_== 1
    result.resultValue.statements.length must_== 1

    result.resultValue.statements(0).isInstanceOf[BeginEndNode] must_== true
  }

  "parse function from file function2.esql" in {
    val input = Source.fromURL(getClass.getResource("/function2.esql")).getLines().mkString("\n")
    val result = ReportingParseRunner(parser.FunctionStatement).run(input)

    result.hasErrors must_== false
    result.resultValue.startLine must_== 1
    result.resultValue.statements.length must_== 1
    result.resultValue.statements(0).isInstanceOf[BeginEndNode] must_== true
  }

  "parse function from file function3.esql" in {
    val input = Source.fromURL(getClass.getResource("/function2.esql")).getLines().mkString("\n")
    val result = ReportingParseRunner(parser.FunctionStatement).run(input)

    result.hasErrors must_== false
    result.resultValue.startLine must_== 1
    result.resultValue.statements.length must_== 1
    result.resultValue.statements(0).isInstanceOf[BeginEndNode] must_== true
  }

}
