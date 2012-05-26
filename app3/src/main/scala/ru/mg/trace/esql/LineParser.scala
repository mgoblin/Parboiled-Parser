package ru.mg.trace.esql

import org.parboiled.scala._

trait LineParser extends Parser {

  def inputLine = rule { Line ~ EOI }

  def Line = rule { Header ~ Node /* ~ StatementText ~ SyntacticPath ~ RelativeLineNum*/ }

  def Header = rule { zeroOrMore(!":" ~ ANY) ~ ":" ~ WS }

  def Node = rule { "Node" ~ WS ~ "'" ~ NodeName ~> { name => name.toString } ~ "':" ~ WS }

  def NodeName = rule { zeroOrMore(!"':" ~ ANY) }

  // Whitespace rule
  def WS: Rule0 = rule {
    zeroOrMore(anyOf(" \t\n\r\f"))
  }

  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ WS
    else
      str(string)


}
