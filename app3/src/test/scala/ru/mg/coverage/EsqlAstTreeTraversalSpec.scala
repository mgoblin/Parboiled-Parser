package ru.mg.coverage

import org.specs2.mutable.SpecificationWithJUnit
import ru.mg.parsing.broker.trace.ast.BrokerTraceStatementNode
import ru.mg.coverage.dsl.DSL._


class EsqlAstTreeTraversalSpec extends SpecificationWithJUnit {

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

  "EsqlAstTreeTraversal" should {

    "traverse all esql nodes and return only root nodes" in {
      esqlNodes.size must_== 2
      esqlNodes(0).linesRange.start must_== 1
      esqlNodes(0).linesRange.end must_== 10
      esqlNodes(1).linesRange.start must_== 12
      esqlNodes(1).linesRange.end must_== 50

      val coverageNodes = new EsqlAstTreeTraversal(trace).traverseAndTransformTree(esqlNodes)

      coverageNodes.size must_== 2
    }
  }
}
