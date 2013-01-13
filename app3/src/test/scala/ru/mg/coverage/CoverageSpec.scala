package ru.mg.coverage

import org.specs2.mutable.SpecificationWithJUnit
import ru.mg.parsing.broker.trace.ast.BrokerTraceStatementNode
import ru.mg.coverage.dsl.DSL._
import ru.mg.parsing.broker.trace.parser.BrokerTraceParser
import io.Source
import org.parboiled.scala.parserunners.ReportingParseRunner
import ru.mg.parsing.esql.parser.ESQLParser
import ru.mg.parsing.esql.ast.{DeclareNode, ModuleNode}


class CoverageSpec extends SpecificationWithJUnit {

  val esqlNodes = esql (
    module("module Foo") (
      function("f1", 1 to 10)
    ),
    module ("module Foo2") (
      function("f2", 12 to 20),
      function("f3", 21 to 40),
      function("f4", 41 to 50)
    )
  )

  def parseTrace(fileName: String) = {
    val traceParser = new BrokerTraceParser { override val buildParseTree = true }
    val traceInput = Source.fromURL(getClass.getResource(fileName)).getLines().mkString("\n")
    val traceRun = ReportingParseRunner(traceParser.Trace).run(traceInput)
    traceRun.resultValue
  }

  def parseEsql(fileName: String) = {
    val esqlParser = new ESQLParser { override val buildParseTree = true }
    val esqlFile = Source.fromURL(getClass.getResource(fileName)).getLines().mkString("\n")
    val esqlRun = ReportingParseRunner(esqlParser.ESQLFile).run(esqlFile)
    esqlRun.resultValue
  }



  val trace = new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 1) ::
    new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 2) ::
    new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 4) ::
    new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 15) :: Nil

  "Coverage" should {

    "traverse all esql nodes and return only root nodes" in {
      esqlNodes.size must_== 2
      esqlNodes(0).linesRange.start must_== 1
      esqlNodes(0).linesRange.end must_== 10
      esqlNodes(1).linesRange.start must_== 12
      esqlNodes(1).linesRange.end must_== 50

      val coverageNodes = new Coverage(trace).coverageForEsqNodes(esqlNodes)

      coverageNodes.size must_== 6
    }

    "make coverage for esql for global declarations" in {

      val traceNodes = parseTrace("/traces/traceForStatementFilter.txt")
      val esqlNodes = parseEsql("/esql/esql.txt")

      val coverage = new Coverage(traceNodes)
      val coverageNodes = coverage.coverageForEsqNodes(esqlNodes)

      coverageNodes.isEmpty must_== false

      val rootNodes = coverageNodes.filter { _.esqlNode.parent == None }
      rootNodes.forall { node => esqlNodes.exists { _ == node.esqlNode } } must_== true
      rootNodes.forall { node => if (node.isInstanceOf[ModuleNode]) node.traces.isEmpty else true } must_== true

      val sharedRowNode = rootNodes.find {_.esqlNode.isInstanceOf[DeclareNode] }
      sharedRowNode must_!= None
      sharedRowNode.get.traces.size must_== 1

      val modulesCoverage = rootNodes.filter { _.esqlNode.isInstanceOf[ModuleNode] }
      modulesCoverage.forall { _.traces.isEmpty } must_== true
    }
  }
}
