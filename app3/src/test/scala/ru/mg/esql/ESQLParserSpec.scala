package ru.mg.esql

import ast._
import org.specs._
import org.parboiled.scala.parserunners.ReportingParseRunner
import org.parboiled.scala._
import scala.io.Source

class ESQLParserSpec extends SpecificationWithJUnit {

  val parser = new ESQLParser {
    override val buildParseTree = true
  }

  "ESQL parser" should {

    "parse file variable declaration" in {
      val input = "declare CacheQueueTable SHARED ROW;"
      val result = ReportingParseRunner(parser.Statement).run(input)

      result.hasErrors mustBe false
      result.resultValue.text must_== "CacheQueueTable SHARED ROW"
    }

    "parse file with line comment" in {
      val input = "-- This is a comment"
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors mustBe false
      result.parseTreeRoot mustNotBe null
      result.parseTreeRoot.getChildren mustNotBe empty

    }

    "parse file with variable declarations and line comments" in {
      val input =
        """-- This is a comment
        declare CacheQueueTable SHARED ROW; -- a shared variable that can be used by instances of a flow
        """
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors mustBe false
      result.parseTreeRoot mustNotBe null
      result.parseTreeRoot.getChildren mustNotBe empty
    }

    "parse file with module declaration" in {
      val input =
        "CREATE COMPUTE MODULE Routing_using_memory_cache_Compute \n" +
          "END MODULE;\n\n"
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors mustBe false
      result.parseTreeRoot mustNotBe null
      result.parseTreeRoot.getChildren mustNotBe empty

    }

    "parse esql.txt" in {
      val input = Source.fromURL(getClass.getResource("/esql.txt")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 4
      result.resultValue(0).isInstanceOf[LineStatementNode] must_== true
      result.resultValue(1).isInstanceOf[LineCommentNode] must_== true
      result.resultValue(2).isInstanceOf[ModuleNode] must_== true
      result.resultValue(3).isInstanceOf[ModuleNode] must_== true

    }

    "parse samples/ExceptionManager.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/ExceptionManager.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 3
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[LineCommentNode] must_== true
      result.resultValue(2).isInstanceOf[FunctionNode] must_== true

    }

    "parse samples/Logging.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/Logging.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 12
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[LineStatementNode] must_== true
      result.resultValue(6).isInstanceOf[FunctionNode] must_== true

    }

    "parse samples/Recode.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/Recode.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 22
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[PathNode] must_== true
      result.resultValue(4).isInstanceOf[FunctionNode] must_== true

    }

    "parse samples/SettingsCache.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/SettingsCache.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 18
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[LineCommentNode] must_== true
      result.resultValue(8).isInstanceOf[FunctionNode] must_== true

    }

    "parse samples/SettingsCacheSubflow.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/SettingsCacheSubflow.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 8
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[LineStatementNode] must_== true
      result.resultValue(6).isInstanceOf[ModuleNode] must_== true

    }

    "parse samples/SettingsControl.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/SettingsControl.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 4
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[ModuleNode] must_== true
      result.resultValue(2).isInstanceOf[ModuleNode] must_== true

    }

    "parse samples/SettingsUtil.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/SettingsUtil.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 5
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[BlockCommentNode] must_== true
      result.resultValue(2).isInstanceOf[FunctionNode] must_== true

    }

    "parse samples/TechRecordation.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/TechRecordation.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 5
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[LineStatementNode] must_== true
      result.resultValue(2).isInstanceOf[ModuleNode] must_== true

    }

    "parse samples/Utils.esql" in {
      val input = Source.fromURL(getClass.getResource("/samples/Utils.esql")).getLines().mkString("\n")
      val result = ReportingParseRunner(parser.ESQLFile).run(input)

      result.hasErrors must_== false
      result.resultValue.length must_== 24
      result.resultValue(0).isInstanceOf[SchemaNode] must_== true
      result.resultValue(1).isInstanceOf[LineStatementNode] must_== true
      result.resultValue(2).isInstanceOf[LineStatementNode] must_== true

    }
  }
}