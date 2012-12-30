package ru.mg.coverage

import ast.CoverageNode
import org.specs2.mutable.SpecificationWithJUnit
import ru.mg.parsing.ast.ModuleNode
import ru.mg.parsing.broker.trace.ast.BrokerTraceStatementNode


class TraceMatcherSpec extends SpecificationWithJUnit {
  "TraceMatcher" should {
    "link esql with traces" in {
      val esql = new ModuleNode("module foo", 3 to 4, List()) :: new ModuleNode("module foo2", 9 to 15, List()) :: Nil
      val trace = new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 1) ::
                  new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 2) ::
                  new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 4) ::
                  new BrokerTraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 15) :: Nil
      val nodesWithTraces = TraceMatcher.traverseTree(esql, Nil, esql => new CoverageNode(esql, trace))
      nodesWithTraces must_!= null
    }
  }
}
