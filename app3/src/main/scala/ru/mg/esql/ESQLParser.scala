package ru.mg.esql

import ru.mg.esql.ast.EsqlAstNode._
import org.parboiled.scala._
import ru.mg.esql.ast.EsqlAstNode


trait ESQLParser extends ModuleParser {

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
    PATH ~ Path ~> withContext{ pathNode } ~ StatementDelimiter ~ WS
  }
  
  def Path = rule {
    SchemaName ~ zeroOrMore("," ~ SchemaName) ~ WS
  }

  def SchemaName = rule {
    Identifier ~ zeroOrMore("." ~ Identifier)
  }

  def Statement: Rule1[EsqlAstNode] = rule {
    (FunctionStatement | DeclareStatement | ModuleStatement | Comment) ~ WS
  }
}
