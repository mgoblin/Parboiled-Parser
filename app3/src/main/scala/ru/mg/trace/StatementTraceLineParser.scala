package ru.mg.trace

import org.parboiled.scala._
import ru.mg.ast.StatementTraceAstNode._


trait StatementTraceLineParser extends Parser {

  def StatementTraceLine = rule { StatementLine ~ EOI }

  def StatementLine = rule { oneOrMore(!"Node" ~ ANY) ~ "Node" ~ "'" ~ NodeName ~ "'" ~~> { statementNode }}

  def NodeName = rule { oneOrMore(!"'" ~ ANY) ~> { _.toString } }

  // Whitespace rule
  def WS: Rule0 = rule {
    zeroOrMore(anyOf(" \t\n\r\f"))
  }

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ WS
    else
      str(string)
}
