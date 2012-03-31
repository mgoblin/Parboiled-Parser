package ru.mg.esql

import ast.AstNode
import ru.mg.esql.ast.AstNode._
import org.parboiled.scala._
import statements.ModuleParser


class ESQLParser extends ModuleParser {

  def InputESQL = rule { ESQLFile ~ EOI }
  
  def ESQLFile = rule {
    optional(Schema) ~ optional(PathStatement) ~ zeroOrMore(Statement)  ~~> 
      {(sch, p, st) =>
        val r = if (p.isDefined) p.get :: st else st
        if (sch.isDefined) sch.get :: r else r
      }
  }

  def Schema = rule {
    BROKER ~ SCHEMA ~ SchemaName ~> withContext{ schemaNode } ~ WS
  }

  def PathStatement = rule {
    PATH ~ Path ~ StatementDelimiter ~> withContext{ pathNode } ~ StatementDelimiter ~ WS
  }
  
  def Path = rule {
    SchemaName ~ optional("," ~ SchemaName)
  }

  def SchemaName = rule {
    Identifier ~ zeroOrMore("." ~ Identifier)
  }

  def Statement: Rule1[AstNode] = rule {
    (FunctionStatement | DeclareStatement | ModuleStatement | Comment) ~ WS
  }
}
