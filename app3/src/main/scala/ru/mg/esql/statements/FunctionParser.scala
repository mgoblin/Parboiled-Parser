package ru.mg.esql.statements

import org.parboiled.scala.rules.Rule1
import ru.mg.esql.AstNode._
import ru.mg.esql.FunctionNode
import org.parboiled.scala._


trait FunctionParser extends StatementParser {
  
  def FunctionStatementMask = rule {
    FunctionHeader ~ oneOrMore(ANY) ~ StatementDelimiter
  }

  def FunctionStatement: Rule1[FunctionNode] = rule {
    FunctionHeader ~ FunctionSignature ~ optional(LineComment) ~ FunctionBody ~ StatementDelimiter ~~> withContext(functionNode)
  }
  
  def FunctionHeader: Rule1[String] = rule {
    WS ~ ignoreCase("CREATE") ~> { _.toString } ~ WS ~
      (ignoreCase("PROCEDURE") | ignoreCase("FUNCTION")) ~> { _.toString } ~~> { _ + " " + _ }
  }

  def FunctionSignature: Rule1[String] = rule {
    WS ~ FunctionName ~> { _.toString } ~ WS ~ "(" ~ ")" ~ WS
  }
  
  def FunctionBody = rule {
    WS ~ zeroOrMore((LineStatement | LineComment) ~ WS) ~ WS
  }

  def FunctionName = Identifier
}
