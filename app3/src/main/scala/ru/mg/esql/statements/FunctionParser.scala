package ru.mg.esql.statements

import org.parboiled.scala.rules.Rule1
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.FunctionNode


trait FunctionParser extends CompoundStatementParser {

  def FunctionStatement: Rule1[FunctionNode] = rule {
    WS ~ CREATE ~ (PROCEDURE | FUNCTION) ~ FunctionSignature ~ FunctionBody ~~> withContext(functionNode)
  }

  def FunctionSignature: Rule1[String] = rule {
    def signature =  { (name: String, params: String, ret: Option[String], language: Option[String]) =>
      name + params + ret.getOrElse("") + language.getOrElse("")
    }

    FunctionName ~ ParamsDecl ~ optional(FunctionReturn) ~ optional(FunctionLanguage) ~~> signature
  }

  
  def ParamsDecl: Rule1[String] = rule {
    "(" ~ zeroOrMore(noneOf(")")) ~> { "(" + _ + ")" } ~ ")" ~ WS
  }
  
  def FunctionLanguage: Rule1[String] = rule {
    LANGUAGE ~ (ESQL | DATABASE | JAVA) ~> { " LANGUAGE " + _ }
  }
  
  def FunctionReturn: Rule1[String] = rule {
    RETURNS ~ TypeDecl ~> { " RETURNS " + _ }
  }
  
  def FunctionBody = rule {
    zeroOrMore(BeginEndStatement | Comment)
  }

  def FunctionName = rule { Identifier ~> { _.toString } ~ WS }
}
