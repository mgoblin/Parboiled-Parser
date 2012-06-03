package ru.mg.coverage

import org.specs.SpecificationWithJUnit
import ru.mg.esql.ast._
import ru.mg.ast.TraceStatementNode


class CoverageSpec extends SpecificationWithJUnit {
  "Coverage" should {
    "link esql with traces" in {
      val esql = new ModuleNode("module foo", 3 to 4, List()) :: new ModuleNode("module foo2", 9 to 15, List()) :: Nil
      val trace = new TraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 1) ::
                  new TraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 2) ::
                  new TraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 4) ::
                  new TraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 15) :: Nil
      val nodesWithTraces = Coverage.nodesWithTraces(esql, trace)

      nodesWithTraces.size must_== 2
      nodesWithTraces(0)._2 must_== new TraceStatementNode("10-01-2012 22:44:32", "1000", "UserTrace", "myNode", "// hello", "module", 4) :: Nil
    }
  }
}
