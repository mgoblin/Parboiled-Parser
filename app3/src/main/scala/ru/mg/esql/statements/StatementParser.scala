package ru.mg.esql.statements

import org.parboiled.scala._
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.LineStatementNode

/**
 * Line statement and comments parser.
 * Line statement is string with ESQL statement. Delimited by ;
 * Parser does not analyze statement or comment body
 */
trait StatementParser extends Parser {

  /**
   * <p>Line statement rule.</p>
   * <p>Support statements with "\n" inside - multi line statements</p>
   *
   * @return AST LineStatementNode
   */
  def LineStatement: Rule1[LineStatementNode] = rule {
    WS ~ oneOrMore(Word) ~ WS ~ StatementDelimiter ~ optional(Comment) ~~> withContext(lineStatementNode)
  }

  def Comment = rule { LineComment | BlockComment }
  
  /**
   * <p> Line comment rule. </p>
   * <p>
   * Line comments starts with -- <br>
   * For example:
   * -- comment
   * </p>
   * Parse to LineComment AST node
   * @see ru.mg.esql.ast.CommentNode
   *
   * @return CommentNode AST
   */
  def LineComment = rule {
    WS ~ "--" ~ zeroOrMore(noneOf("\n")) ~> withContext(commentNode) ~ optional("\n")
  }

  def BlockComment = rule {
    WS ~ "/**" ~ zeroOrMore(!"**/" ~ ANY) ~> withContext(blockCommentNode) ~ "**/" ~ WS
  }

  protected def Word = rule {
    oneOrMore(noneOf(StatementDelimiter)) ~? ModuleUtils.isNotModuleOrFunction ~> { _.toString } }

  def StatementDelimiter = ";"

  def Identifier = rule {
    oneOrMore("a" - "z" | "A" - "Z" | "0" - "9" | "_" )
  }

  // Whitespace rule
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
