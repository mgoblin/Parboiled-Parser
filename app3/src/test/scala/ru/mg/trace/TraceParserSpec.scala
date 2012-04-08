package ru.mg.trace

import org.specs.SpecificationWithJUnit
import ru.mg.ast.TraceLineNode
import io.Source
import org.parboiled.scala.parserunners.ReportingParseRunner


class TraceParserSpec extends SpecificationWithJUnit {

  val parser = new TraceParser { override val buildParseTree = true }

  "Trace line rule" should {

    "parse trace line to AST" in {
      val input = "2008-04-23 14:22:40.014060     2864   UserTrace   BIP6060I: Parser type ''Properties'' created on behalf of node 'Routing_using_memory_cache.Queue: ROUTING.MEMORY.IN1' to handle portion of incoming message of length 0 bytes beginning at offset '0'."

      val run = ReportingParseRunner(parser.TraceLine).run(input)
      val result = run.resultValue

      result.isInstanceOf[TraceLineNode] mustBe true
      result.timestamp must_== "2008-04-23 14:22:40.014060"
      result.threadId must_== "2864"
      result.traceType must_== "UserTrace"
      result.message must_== "BIP6060I: Parser type ''Properties'' created on behalf of node 'Routing_using_memory_cache.Queue: ROUTING.MEMORY.IN1' to handle portion of incoming message of length 0 bytes beginning at offset '0'."
    }
  }
  "Trace parser" should {

    "parse trace line sample" in {
      val input = Source.fromURL(getClass.getResource("/traces/trace.txt")).getLines().mkString("\n")

      val run = ReportingParseRunner(parser.Trace).run(input)
      val result = run.resultValue

      result mustNotBe null
      result.length must_== 820
    }

  }
}
