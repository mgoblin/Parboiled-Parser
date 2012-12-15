package ru.mg.parsing.broker.trace.parser

import org.parboiled.scala._
import parts.common.TimestampParser
import ru.mg.parsing.broker.trace.ast.{TraceLineNode, TraceAstNode}
import ru.mg.parsing.broker.trace.ast.TraceAstNode._


trait TraceParser extends TimestampParser {

  def Trace = rule { zeroOrMore(TraceLine) ~ EOI }

  def TraceLine: Rule1[TraceLineNode] = rule {
    Timestamp ~ WS ~ ThreadId ~ WS ~ TraceType ~ WS ~ Message ~~> { traceLineNode } ~ WS
  }

  def ThreadId = rule { oneOrMore("0" - "9") ~> { _.toString } }

  def TraceType = rule { oneOrMore(("a" - "z") | ("A" - "Z")) ~> { _.toString } }

  def Message = rule { zeroOrMore(!(Timestamp) ~ ANY) ~> { _.toString } }

  def WS: Rule0 = rule {
    zeroOrMore(anyOf(" \t\n\r\f"))
  }

}
