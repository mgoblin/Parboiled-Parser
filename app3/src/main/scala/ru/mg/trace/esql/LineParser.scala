package ru.mg.trace.esql

import org.parboiled.scala._

trait LineParser extends Parser {

  def inputLine = rule { Line ~ EOI }

  def Line = rule { Header ~ Node ~ StatementText ~ SyntacticPath ~ Position }

  def Header = rule { zeroOrMore(!":" ~ ANY) ~ ":" ~ WS }

  def Node = rule { "Node" ~ WS ~ "'" ~ NodeName ~> { _.toString } ~ "':" ~ WS }

  def NodeName = rule { zeroOrMore(!"':" ~ ANY) }

  def StatementText = rule { "Executing statement" ~ WS ~ "''" ~ Text ~> { _.toString } ~ "''" ~ WS }

  def Text = rule { zeroOrMore(!"''" ~ ANY) }

  def SyntacticPath = rule { "at ('" ~ ModulePart ~> { _.toString } ~ "'," ~ WS }

  def ModulePart = rule { zeroOrMore(!"'" ~ ANY) }

  def Position = rule { "'" ~ LineNums ~ "')." ~ WS }

  def LineNums = rule { num ~> { _.toInt } ~ "." ~ num }

  def num = rule { oneOrMore("0" - "9") }

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
