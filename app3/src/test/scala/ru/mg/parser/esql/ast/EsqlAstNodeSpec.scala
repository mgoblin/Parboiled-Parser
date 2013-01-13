package ru.mg.parser.esql.ast

import org.specs2.mutable.SpecificationWithJUnit
import ru.mg.parsing.esql.ast.{ModuleNode, FunctionNode}
import ru.mg.parsing.esql.parser.ESQLParser
import org.parboiled.scala.parserunners.ReportingParseRunner


class EsqlAstNodeSpec extends SpecificationWithJUnit {

  "EsqlAstNode" should {

    "calculate code path for leaf nodes" in {
      val functionNode = new FunctionNode("function1", "function1()", 1 to 10, Nil)
      functionNode.codePart must_== "." + functionNode.text
    }

    "calculate code path for module functions" in {
      import ru.mg.coverage.dsl.DSL._

      val module1: ModuleNode = module("module1") {
        function("function1", 2 to 5)
      }
      val function1 = module1.statements(0)

      function1.codePart must_== "." + module1.text + "." + function1.text
    }

    "calculate code path for root shared row declaration" in {
      val sharedRowDecl = "declare CacheQueueTable SHARED ROW;"
      val esqlParser = new ESQLParser { override val buildParseTree = true }
      val esqlRun = ReportingParseRunner(esqlParser.ESQLFile).run(sharedRowDecl)
      val esqlNode = esqlRun.resultValue(0)

      esqlNode.codePart must_==  ".CacheQueueTable"
    }
  }

}
