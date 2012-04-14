package ru.mg.trace

import org.parboiled.scala._
import ru.mg.ast.StatementTraceAstNode._


trait StatementTraceLineParser extends Parser {

  def StatementTraceLine = rule { StatementLine ~ EOI }

  def StatementLine = rule {
    Node ~ StatementTrace ~ CodePlace ~~> { statementNode }
  }

  def Node: Rule1[String] = rule {
    Prefix ~  WS ~ "'" ~ NodeName ~> { _.toString } ~ "'" ~ WS
  }

  def StatementTrace: Rule1[String] = rule {
    ":" ~ WS ~ "Executing" ~ WS ~ "statement" ~ WS ~ "''" ~ Statement ~> { _.toString } ~ "''" ~ WS
  }

  def CodePlace = rule {
    "at" ~ WS ~ "(" ~ WS ~ "'" ~ CodePartName ~> { _.toString } ~ "'" ~ WS
  }

  def CodePartName = rule { rule { oneOrMore(!"'" ~ ANY) } }
  def NodeName = rule { oneOrMore(!"'" ~ ANY) }
  def Statement = rule { oneOrMore(!"''" ~ ANY) }

  def Prefix = rule { oneOrMore(!"Node" ~ ANY) ~ "Node" }

  // Whitespace rule
  def WS: Rule0 = rule {
    zeroOrMore(anyOf(" \t\n\r\f"))
  }

}
