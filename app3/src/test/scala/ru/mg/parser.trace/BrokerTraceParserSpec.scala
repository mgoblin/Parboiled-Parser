package ru.mg.trace

import io.Source
import org.parboiled.scala.parserunners.{Rules, TracingParseRunner, ReportingParseRunner}
import ru.mg.parsing.broker.trace.ast.{BrokerTraceStatementNode, BrokerTraceLineNode}
import ru.mg.parsing.broker.trace.parser.BrokerTraceParser
import org.specs2.mutable.SpecificationWithJUnit


class BrokerTraceParserSpec extends SpecificationWithJUnit {

  val parser = new BrokerTraceParser { override val buildParseTree = true }

  "CommonTraceLine rule" should {

    "parse non statement line to AST" in {
      val input = "2008-04-23 14:22:25.473077      400   UserTrace   BIP6060I: Parser type ''Properties'' created on behalf of node 'Routing_using_memory_cache.Queue: ROUTING.REFRESH.IN1' to handle portion of incoming message of length 0 bytes beginning at offset '0'."

      val run = ReportingParseRunner(parser.CommonTraceLine).run(input)
      val result = run.resultValue

      result.timestamp must_== "2008-04-23 14:22:25.473077"
      result.threadId must_== "400"
      result.traceType must_== "UserTrace"
      result.message must_== "BIP6060I: Parser type ''Properties'' created on behalf of node 'Routing_using_memory_cache.Queue: ROUTING.REFRESH.IN1' to handle portion of incoming message of length 0 bytes beginning at offset '0'."
    }
  }

  "StatementTraceLine rule" should {

    "parse statement line to AST" in {
      val input = "2008-04-23 14:22:25.480670      400   UserTrace   BIP2537I: Node 'Routing_using_memory_cache.Refresh memory cache': Executing statement   ''DECLARE CacheQueueTable ROW;'' at ('.CacheQueueTable', '1.1').\n"
      val run = ReportingParseRunner(parser.StatementTraceLine).run(input)
      val node = run.resultValue

      node must_!= null

      node.timestamp must_== "2008-04-23 14:22:25.480670"
      node.threadId must_== "400"
      node.traceType must_== "UserTrace"
      node.nodeName must_== "Routing_using_memory_cache.Refresh memory cache"
      node.statement must_== "DECLARE CacheQueueTable ROW;"
      node.codePart must_== ".CacheQueueTable"
      node.esqlLineNo must_== 1

    }
  }

  "Trace parser rule" should {

    "parse traceForStatementsFilter.txt" in {
      val input = Source.fromURL(getClass.getResource("/traces/traceForStatementFilter.txt")).getLines().mkString("\n")

      val run = ReportingParseRunner(parser.Trace).run(input)
      val result = run.resultValue

      result must_!= null
      result.length must_== 13

      val statements = result.filter(_.isInstanceOf[BrokerTraceStatementNode])
      statements.length must_== 6
    }

    "parse parse.txt" in {
      val input = Source.fromURL(getClass.getResource("/traces/trace.txt")).getLines().mkString("\n")

      val run = ReportingParseRunner(parser.Trace).run(input)
      val result = run.resultValue

      result must_!= null
      result.length must_== 820

      val statements = result.filter(node => node.isInstanceOf[BrokerTraceStatementNode])
      statements.length must_== 64
    }

  }
}
