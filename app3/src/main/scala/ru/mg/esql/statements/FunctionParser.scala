package ru.mg.esql.statements

import org.parboiled.scala.rules.Rule1
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.FunctionNode
import org.parboiled.scala._


trait FunctionParser extends StatementParser {
  
  def FunctionStatementMask = rule {
    FunctionHeader ~ oneOrMore(ANY) ~ StatementDelimiter
  }

  def FunctionStatement: Rule1[FunctionNode] = rule {
    FunctionHeader ~ FunctionSignature ~ optional(Comment) ~ FunctionBody ~ StatementDelimiter ~~> withContext(functionNode)
  }
  
  def FunctionHeader: Rule1[String] = rule {
    WS ~ ignoreCase("CREATE") ~> { _.toString } ~ WS ~
      (ignoreCase("PROCEDURE") | ignoreCase("FUNCTION")) ~> { _.toString } ~~> { _ + " " + _ }
  }

  def FunctionSignature: Rule1[String] = rule {
    WS ~ FunctionName ~ ParamsDecl ~~> { (a, b) => a + b } ~ WS
  }
  
  def ParamsDecl: Rule1[String] = rule {
    WS ~ "(" ~ zeroOrMore(!")" ~ ANY) ~> { "(" + _.toString + ")" } ~ ")" ~ WS
  }
  
  def FunctionBody = rule {
    WS ~ zeroOrMore((LineStatement | Comment) ~ WS) ~ WS
  }

  def FunctionName: Rule1[String] = rule { Identifier ~> { _.toString } }
}
