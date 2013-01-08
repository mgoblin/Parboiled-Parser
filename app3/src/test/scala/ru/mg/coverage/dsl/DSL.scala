package ru.mg.coverage.dsl

import ru.mg.parsing.esql.ast.{FunctionNode, ModuleNode, EsqlAstNode}


object DSL {

  private def linesRange(statements: Seq[EsqlAstNode]) = {
    statements match {
      case List() | Nil => 0 to 0
      case _ => statements.minBy(_.linesRange.start).linesRange.start to statements.maxBy(_.linesRange.end).linesRange.end
    }
  }

  def esql (statements: EsqlAstNode*) = { statements.toList }

  def module(name: String) (statements: EsqlAstNode*): ModuleNode = {
    new ModuleNode(
      name,
      name,
      linesRange(statements),
      statements.toList)
  }

  def  function(name: String, range: Range) = {
    new FunctionNode(name, name, range, Nil)
  }

}
