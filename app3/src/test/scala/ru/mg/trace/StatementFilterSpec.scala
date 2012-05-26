package ru.mg.trace

import org.specs.SpecificationWithJUnit
import io.Source
import org.parboiled.scala.parserunners.ReportingParseRunner


class StatementFilterSpec extends SpecificationWithJUnit {

  val filter = new StatementFilter()
  val parser = new TraceParser { override val buildParseTree = true }

  "Statement filter" should {
    "filter not statement lines from trace" in {
      val inputStrings = Source.fromURL(getClass.getResource("/traces/traceForStatementFilter.txt")).getLines().mkString("\n")
      val inputTrace = ReportingParseRunner(parser.Trace).run(inputStrings).resultValue

      inputTrace mustNotBe null

      val statements = filter.filterStatementExecutions(inputTrace)


      statements mustNotBe empty

      statements.size must_== 7

      statements(0).timestamp must_== "2008-04-23 14:22:25.480670"
      statements(0).threadId must_== "400"
      statements(0).traceType must_== "UserTrace"
      statements(0).nodeName must_== "Routing_using_memory_cache.Refresh memory cache"
      statements(0).statement must_== "DECLARE CacheQueueTable ROW;"
      statements(0).codePart must_== ".CacheQueueTable"
      statements(0).esqlLineNo must_== 1
    }
  }
}
