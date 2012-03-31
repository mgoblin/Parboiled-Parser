package ru.mg.esql

import ast.AstNode
import org.parboiled.scala._
import statements.ModuleParser


class ESQLParser extends ModuleParser {

  def InputESQL = rule { ESQLFile ~ EOI }
  
  def ESQLFile:Rule1[List[AstNode]] = rule { zeroOrMore(Statement) }

  def Statement: Rule1[AstNode] = rule {
    (ModuleStatement| FunctionStatement | Comment | DeclareStatement) ~ WS
  }
}
