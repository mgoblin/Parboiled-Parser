package ru.mg.esql.statements

import org.parboiled.scala.rules.Rule1
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.FunctionNode
import org.parboiled.scala._


trait FunctionParser extends CompoundStatementParser {

  def FunctionStatementMask = rule {
    FunctionHeader ~ oneOrMore(ANY) ~ StatementDelimiter
  }

  def FunctionStatement: Rule1[FunctionNode] = rule {
    FunctionHeader ~ FunctionSignature ~ optional(Comment) ~ FunctionBody ~~> withContext(functionNode)
  }
  
  def FunctionHeader: Rule1[String] = rule {
    WS ~ ignoreCase("CREATE") ~> { _.toString } ~ WS ~
      (ignoreCase("PROCEDURE") | ignoreCase("FUNCTION")) ~> { _.toString } ~~> { _ + " " + _ }
  }

  def FunctionSignature: Rule1[String] = rule {
    def signature =  { (name: String, params: String, ret: Option[String], language: Option[String]) =>
      name + params + conv(ret) + conv(language)
    }

    def conv(x: Option[String]): String = x match {
      case Some(v) => " " + v
      case None => ""
    }

    WS ~ FunctionName ~ WS ~ ParamsDecl ~ WS ~ optional(FunctionReturn) ~ WS ~ optional(FunctionLanguage) ~~>
      signature

  }

  
  def ParamsDecl: Rule1[String] = rule {
    "(" ~ zeroOrMore(noneOf(")")) ~> { "(" + _ + ")" } ~ ")"
  }
  
  def FunctionLanguage: Rule1[String] = rule {
    LANGUAGE ~ WS ~ (ESQL | DATABASE | JAVA) ~> { "LANGUAGE " + _ }
  }
  
  def FunctionReturn: Rule1[String] = rule {
    RETURNS ~ WS ~ TypeDecl ~> { "RETURNS " + _ }
  }
  
  def FunctionBody = rule {
    zeroOrMore(BeginEnd | Comment)
  }

  def FunctionName = rule { Identifier ~> { _.toString } }
}
