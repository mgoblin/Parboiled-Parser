package ru.mg.esql.statements

import org.parboiled.scala._
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.{BeginEndNode, LineStatementNode}


trait StatementParser extends ReservedWordsParser {

  def BeginEndStatement: Rule1[BeginEndNode] = rule {
    BEGIN ~
      zeroOrMore(!(END ~ StatementDelimiter) ~ ANY) ~> withContext{ beginEndNode } ~
    END ~ StatementDelimiter
  }

  def LineStatement: Rule1[LineStatementNode] = rule {
    zeroOrMore(!(StatementDelimiter) ~ ANY) ~> withContext(lineStatementNode) ~ StatementDelimiter
  }

  def Comment = rule { LineComment | BlockComment }

  def LineComment = rule {
    "--" ~ zeroOrMore(!(NewLine) ~ ANY) ~> withContext(commentNode) ~ NewLine
  }

  def BlockComment = rule {
    "/*" ~ zeroOrMore(!("*/") ~ ANY) ~> withContext(blockCommentNode) ~ "*/"
  }

  def StatementDelimiter = ignoreCase(";")
  def NewLine = optional("\r") ~ "\n"

}
