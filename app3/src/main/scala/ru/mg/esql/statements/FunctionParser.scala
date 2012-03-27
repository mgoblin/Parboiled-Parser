package ru.mg.esql.statements

import org.parboiled.scala._
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.{AstNode, FunctionNode}


trait FunctionParser extends StatementParser {

  def FunctionStatement: Rule1[FunctionNode] = rule {
    CREATE ~ (PROCEDURE | FUNCTION) ~ FunctionSignature ~ FunctionBody ~~> withContext(functionNode)
  }

  def FunctionSignature: Rule1[String] = rule {
    def signature =  { (name: String, params: String, ret: Option[String], language: Option[String]) =>
      name + params + ret.getOrElse("") + language.getOrElse("")
    }

    FunctionName ~ ParamsDecl ~ optional(FunctionReturn) ~ optional(FunctionLanguage) ~~> signature
  }

  
  def ParamsDecl: Rule1[String] = rule {
    "(" ~ zeroOrMore(!")" ~ ANY) ~> { "(" + _ + ")" } ~ ")" ~ WS
  }
  
  def FunctionLanguage: Rule1[String] = rule {
    LANGUAGE ~ (ESQL | DATABASE | JAVA) ~> { " LANGUAGE " + _ } ~ WS
  }
  
  def FunctionReturn: Rule1[String] = rule {
    RETURNS ~ TypeDecl ~> { " RETURNS " + _ } ~ WS
  }
  
  def FunctionBody: Rule1[List[AstNode]] = rule {
    oneOrMore((BeginEndStatement | Comment) ~ WS) |
    ignoreCase ("EXTERNAL") ~ ignoreCase ("NAME") ~ Identifier ~> withContext { externalNode }
  }

  def FunctionName = rule { Identifier ~> { _.toString } }
}
