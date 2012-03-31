package ru.mg.esql

import org.parboiled.scala._
import statements.ModuleParser


class ESQLParser extends ModuleParser {

  def InputESQL = rule { File ~ EOI }
  
  def File = rule { WS ~ zeroOrMore(Statement) }
  
  def Statement = rule {
    (Comment | FunctionStatement | ModuleStatement | LineStatement) ~ WS

  }

}
