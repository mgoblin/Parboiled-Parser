package ru.mg.esql.statements

import org.parboiled.scala._
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.BeginEndNode


trait StatementParser extends ReservedWordsParser {

  def BeginEndStatement: Rule1[BeginEndNode] = rule {
    BEGIN ~
      zeroOrMore(!(End) ~ ANY) ~> withContext{ beginEndNode } ~
    End
  }

  def End = NewLine ~ zeroOrMore(anyOf(" \t"))  ~ END ~ StatementDelimiter

  def Comment = rule { LineComment | BlockComment }

  def LineComment = rule {
    "--" ~ zeroOrMore(!(NewLine) ~ ANY) ~> withContext(commentNode) ~ optional(NewLine)
  }

  def BlockComment = rule {
    "/*" ~ zeroOrMore(!("*/") ~ ANY) ~> withContext(blockCommentNode) ~ "*/"
  }

  def DeclareStatement = rule {
    DECLARE ~ zeroOrMore(!StatementDelimiter ~ ANY) ~> withContext { lineStatementNode } ~ StatementDelimiter
  }

  def StatementDelimiter = ignoreCase(";")
  def NewLine = optional("\r") ~ "\n"

}
