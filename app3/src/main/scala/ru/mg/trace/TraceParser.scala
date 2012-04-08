package ru.mg.trace

import org.parboiled.scala._
import ru.mg.ast.TraceAstNode._
import ru.mg.ast.TraceLineNode


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
