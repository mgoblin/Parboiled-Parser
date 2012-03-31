package ru.mg.esql

import org.specs._
import org.parboiled.scala.parserunners.ReportingParseRunner
import org.parboiled.scala._

class ESQLParserSpec extends SpecificationWithJUnit {

  val parser = new ESQLParser {
    override val buildParseTree = true
  }

  def parse(input: String) = {
    ReportingParseRunner(parser.InputESQL).run(input)
  }

////  "ESQL parser" should {
////
////    "parse file variable declaration" in {
////      val input = "declare CacheQueueTable SHARED ROW;"
////      val result = parse(input)
////
////      result.hasErrors mustBe false
////      result.parseTreeRoot mustNotBe null
////      result.parseTreeRoot.getChildren mustNotBe empty
////    }
////
////    "parse file with line comment" in {
////      val input = "-- This is a comment"
////      val result = parse(input)
////
////      result.hasErrors mustBe false
////      result.parseTreeRoot mustNotBe null
////      result.parseTreeRoot.getChildren mustNotBe empty
////
////    }
////
////    "parse file with variable declarations and line comments" in {
////      val input =
////        """
////        -- This is a comment
////        declare CacheQueueTable SHARED ROW; -- a shared variable that can be used by instances of a flow
////        """
////      val result = parse(input)
////
////      result.hasErrors mustBe false
////      result.parseTreeRoot mustNotBe null
////      result.parseTreeRoot.getChildren mustNotBe empty
////    }
////
////    "parse file with module declaration" in {
////      val input =
////        "CREATE COMPUTE MODULE Routing_using_memory_cache_Compute \n" +
////          "END MODULE;\n\n"
////      val result = parse(input)
////
////      result.hasErrors mustBe false
////      result.parseTreeRoot mustNotBe null
////      result.parseTreeRoot.getChildren mustNotBe empty
////
////    }
//
//  }
}