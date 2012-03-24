package ru.mg.esql.statements

import org.parboiled.scala._
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.LineStatementNode

/**
 * Line statement and comments parser.
 * Line statement is string with ESQL statement. Delimited by ;
 * Parser does not analyze statement or comment body
 */
trait StatementParser extends ReservedWordsParser {

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
    oneOrMore(noneOf(StatementDelimiter)) ~? ModuleUtils.isLineStatement ~> { _.toString } }

  def StatementDelimiter = ";"

}
