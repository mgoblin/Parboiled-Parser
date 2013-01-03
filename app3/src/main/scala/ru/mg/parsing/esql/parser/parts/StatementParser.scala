package ru.mg.parsing.esql.parser.parts

import org.parboiled.scala._
import ru.mg.parsing.esql.ast.{BeginEndNode, EsqlAstNode}
import EsqlAstNode._


trait StatementParser extends ReservedWordsParser {

  def BeginEndStatement: Rule1[BeginEndNode] = rule {
    BEGIN ~
      zeroOrMore(!(End) ~ ANY) ~> withContext { beginEndNode } ~
    End
  }

  def End = NewLine ~ zeroOrMore(anyOf(" \t")) ~ END ~ StatementDelimiter

  def Comment = rule {
    LineComment | BlockComment
  }

  def LineComment = rule {
    "--" ~ zeroOrMore(!(NewLine) ~ ANY) ~> withContext(commentNode) ~ optional(NewLine)
  }

  def BlockComment = rule {
    "/*" ~ zeroOrMore(!("*/") ~ ANY) ~> withContext(blockCommentNode) ~ "*/" ~ WS
  }

  def DeclareStatement = rule {
    DECLARE ~ zeroOrMore(!StatementDelimiter ~ ANY) ~> withContext {
      lineStatementNode
    } ~ StatementDelimiter
  }

  def StatementDelimiter = ignoreCase(";")

  def NewLine = optional("\r") ~ "\n"

}
