package ru.mg.trace

import org.specs.SpecificationWithJUnit
import org.parboiled.scala.parserunners.ReportingParseRunner


class StatementTraceLineParserSpec extends SpecificationWithJUnit {

  val parser = new StatementTraceLineParser { override val buildParseTree = true }

  "Statement trace line parser" should {
    "parse statement trace line" in {
      val input = "BIP2537I: Node 'Routing_using_memory_cache.Refresh memory cache': Executing statement   ''SET CacheQueueTable.valid VALUE  = NULL;'' at ('.Refresh_memory_cache_Compute.Main', '5.3')."

      val run = ReportingParseRunner(parser.StatementLine).run(input)
      val result = run.resultValue

      run.hasErrors must_== false
      result.nodeName must_== "Routing_using_memory_cache.Refresh memory cache"
      result.statement must_== "SET CacheQueueTable.valid VALUE  = NULL;"
      result.codePart must_== ".Refresh_memory_cache_Compute.Main"
      result.esqlLineNo must_== 5
    }
  }
}
