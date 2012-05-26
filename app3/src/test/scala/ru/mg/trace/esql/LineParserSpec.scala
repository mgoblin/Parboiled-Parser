package ru.mg.trace.esql

import org.specs.SpecificationWithJUnit
import org.parboiled.scala.parserunners.ReportingParseRunner


class LineParserSpec extends SpecificationWithJUnit {

  val parser = new LineParser { override val buildParseTree = true }

  "LineParser" should {
    "parse trace message header" in {
      val input = "UserTrace   BIP6060I: Parser type ''Properties'' created on behalf of node 'Routing_using_memory_cache.Queue: ROUTING.MEMORY.IN1' to handle portion of incoming message of length 0 bytes beginning at offset '0'."

      val run = ReportingParseRunner(parser.Header).run(input)
      run.hasErrors mustBe false

    }

    "parse node name" in {
      val input = "Node 'Routing_using_memory_cache.Find destination from in memory cache': Executing statement   ''DECLARE CacheQueueTable ROW;'' at ('.CacheQueueTable', '1.1')."

      val run = ReportingParseRunner(parser.Node).run(input)
      run.hasErrors mustBe false

      val result = run.resultValue
      result must_== "Routing_using_memory_cache.Find destination from in memory cache"
    }

    "parse statement" in {
      val input = "Executing statement   ''DECLARE CacheQueueTable ROW;'' at ('.CacheQueueTable', '1.1')."

      val run = ReportingParseRunner(parser.StatementText).run(input)
      run.hasErrors mustBe false

      val result = run.resultValue
      result must_== "DECLARE CacheQueueTable ROW;"
    }

    "parse syntactic path" in {
      val input = "at ('.CacheQueueTable', '1.1')."

      val run = ReportingParseRunner(parser.SyntacticPath).run(input)
      run.hasErrors mustBe false

      val result = run.resultValue
      result must_== ".CacheQueueTable"
    }

    "parse position" in {
      val input = "'1.2')."

      val run = ReportingParseRunner(parser.Position).run(input)
      run.hasErrors mustBe false

      val result = run.resultValue
      result must_== 1
    }
  }

}
