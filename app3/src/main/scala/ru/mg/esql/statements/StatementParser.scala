package ru.mg.esql.statements

import org.parboiled.scala._
import ru.mg.esql.AstNode.{commentNode, lineStatementNode}
import ru.mg.esql.LineStatementNode

trait StatementParser extends Parser {

  def LineStatement: Rule1[LineStatementNode] = rule {
    WS ~ oneOrMore(Word) ~ WS ~ StatementDelimiter ~ optional(LineComment) ~~> withContext(lineStatementNode)
  }

  def LineComment = rule {
    WS ~ "--" ~ zeroOrMore(noneOf("\n")) ~> withContext(commentNode) ~ optional("\n")
  }

  protected def Word = rule {
    oneOrMore(noneOf(StatementDelimiter)) ~? ModuleUtils.isNotModuleOrFunction ~> { _.toString } }

  def StatementDelimiter = ";"

  def Identifier = rule {
    oneOrMore("a" - "z" | "A" - "Z" | "0" - "9" | "_")
  }

  protected def WS: Rule0 = rule {
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
