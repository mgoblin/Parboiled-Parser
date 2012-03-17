package ru.mg.esql

import org.parboiled.scala._
import statements.ModuleParser


class ESQLParser extends ModuleParser {

  def InputESQL = rule { File ~ EOI }
  
  def File = rule { zeroOrMore(Statement ~ WS) }
  
  def Statement: Rule1[AstNode] = rule {
    LineStatement | LineComment
  }

}
