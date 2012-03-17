package ru.mg.esql.statements

import org.parboiled.scala._
import ru.mg.esql.AstNode._
import ru.mg.esql.statements.ModuleUtils._


trait ModuleParser extends FunctionParser {
  
  def Module = rule {
    (ModuleHeader ~ WS ~ ModuleBody ~ WS ~ ModuleFooter ~ WS ~ ignoreCase(StatementDelimiter)) ~~> withContext(moduleNode)
  }

  def ModuleHeader = rule {
    WS ~ ignoreCase("CREATE") ~ WS ~ ModuleType ~ WS ~ ignoreCase("MODULE") ~ WS ~ ModuleName ~> normalize
  }

  def ModuleType = rule {
    ignoreCase("COMPUTE") | ignoreCase("DATABASE") | ignoreCase("FILTER")
  }

  def ModuleFooter = rule {
    (ignoreCase("END") ~ WS ~ ignoreCase("MODULE")) ~> normalize
  }

  def ModuleBody = rule {
    zeroOrMore((LineComment | LineStatement) ~ WS)
  }

  def ModuleName = Identifier

}
