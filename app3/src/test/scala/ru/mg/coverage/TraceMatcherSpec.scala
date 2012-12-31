package ru.mg.coverage

import ast.CoverageNode
import org.specs2.mutable.SpecificationWithJUnit
import ru.mg.parsing.ast.EsqlAstNode
import ru.mg.parsing.broker.trace.ast.BrokerTraceStatementNode
import ru.mg.coverage.dsl.DSL._


class TraceMatcherSpec extends SpecificationWithJUnit {

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

  val trace = new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 1) ::
    new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 2) ::
    new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 4) ::
    new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 15) :: Nil

  "TraceMatcher" should {

    "traverse all esql nodes" in {
      esqlNodes.size must_== 2
      esqlNodes(0).linesRange.start must_== 1
      esqlNodes(0).linesRange.end must_== 10
      esqlNodes(1).linesRange.start must_== 12
      esqlNodes(1).linesRange.end must_== 50

      var traversedNodes = List[EsqlAstNode]()
      val coverageNodes = new TraceMatcher(trace).traverseTree(esqlNodes, Nil, esql => {
        traversedNodes = esql :: traversedNodes
        new CoverageNode(esql, trace)
      })

      coverageNodes.size must_== 6
    }

    "link esql with traces" in {

      val coverageNodes = new TraceMatcher(trace).traverseTree(esqlNodes, Nil, esql => new CoverageNode(esql, trace))
      coverageNodes must_!= null
    }
  }
}
