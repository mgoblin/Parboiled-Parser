package ru.mg.parsing.esql.parts

import org.parboiled.scala._
import ru.mg.parsing.ast.{FunctionNode, EsqlAstNode}
import EsqlAstNode._


trait FunctionParser extends StatementParser {

  def FunctionStatement: Rule1[FunctionNode] = rule {
    CREATE ~ (PROCEDURE | FUNCTION) ~ FunctionSignature ~ FunctionBody ~~> withContext(functionNode)
  }

  def FunctionSignature: Rule1[String] = rule {
    def signature = {
      (name: String, params: String, ret: Option[String], language: Option[String]) =>
        name + params + ret.getOrElse("") + language.getOrElse("")
    }

    FunctionName ~ ParamsDecl ~ optional(FunctionReturn) ~ optional(FunctionLanguage) ~~> signature
  }


  def ParamsDecl: Rule1[String] = rule {
    "(" ~ zeroOrMore(!")" ~ ANY) ~> {
      "(" + _ + ")"
    } ~ ")" ~ WS
  }

  def FunctionLanguage: Rule1[String] = rule {
    LANGUAGE ~ (ESQL | DATABASE | JAVA) ~> {
      " LANGUAGE " + _
    } ~ WS
  }

  def FunctionReturn: Rule1[String] = rule {
    RETURNS ~ TypeDecl ~> {
      " RETURNS " + _
    } ~ WS
  }

  def FunctionBody: Rule1[List[EsqlAstNode]] = rule {
    oneOrMore((BeginEndStatement | Comment) ~ WS) |
      EXTERNAL ~ NAME ~ "\"" ~ oneOrMore(!"\"" ~ ANY) ~> withContext {
        externalNode
      } ~ "\"" ~ StatementDelimiter
  }

  def FunctionName = rule {
    Identifier ~> {
      _.toString
    } ~ WS
  }
}
