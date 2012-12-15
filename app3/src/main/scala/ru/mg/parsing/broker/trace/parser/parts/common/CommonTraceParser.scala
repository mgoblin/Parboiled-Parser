package ru.mg.parsing.broker.trace.parser.parts.common

import org.parboiled.scala._
import ru.mg.parsing.broker.trace.ast.BrokerTraceLineNode
import ru.mg.parsing.broker.trace.ast.BrokerTraceAstNode._
import ru.mg.parsing.broker.trace.parser.parts.TimestampParser


trait CommonTraceParser extends TimestampParser {

  def Trace = rule { zeroOrMore(TraceLine) ~ EOI }

  def TraceLine: Rule1[BrokerTraceLineNode] = rule {
    Timestamp ~ WS ~ ThreadId ~ WS ~ TraceType ~ WS ~ Message ~~> { brokerTraceLineNode } ~ WS
  }

  def TraceHeader = rule {
    Timestamp ~ WS ~ ThreadId ~ WS ~ TraceType ~ WS
  }

  def ThreadId = rule { oneOrMore("0" - "9") ~> { _.toString } }

  def TraceType = rule { oneOrMore(("a" - "z") | ("A" - "Z")) ~> { _.toString } }

  def Message = rule { zeroOrMore(!(Timestamp) ~ ANY) ~> { _.toString } }

  def WS: Rule0 = rule {
    zeroOrMore(anyOf(" \t\n\r\f"))
  }

}
