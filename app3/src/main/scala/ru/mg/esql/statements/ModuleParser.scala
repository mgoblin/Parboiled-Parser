package ru.mg.esql.statements

import ru.mg.esql.ast.AstNode._
import org.parboiled.scala._


trait ModuleParser extends FunctionParser {
  
  def ModuleStatement = rule {
    (ModuleHeader ~ ModuleBody ~ ModuleFooter ~ StatementDelimiter) ~~> withContext(moduleNode)
  }

  def ModuleHeader = rule {
    def join = { x: String => x.split(" ").map(_.trim()).filter(!_.isEmpty).mkString(" ") }
    CREATE ~ ModuleType ~ MODULE ~ ModuleName ~ WS ~> join
  }

  def ModuleType = rule {
    (ignoreCase("COMPUTE") | ignoreCase("DATABASE") | ignoreCase("FILTER")) ~ WS
  }

  def ModuleFooter = rule {
    ignoreCase("END") ~ WS ~ ignoreCase("MODULE") ~ WS ~> { _ => "END MODULE" }
  }

  def ModuleBody = rule {
    zeroOrMore(FunctionStatement | Comment | moduleLineStatement) ~ WS
  }

  def moduleLineStatement = rule {
    zeroOrMore(!(ModuleFooter) ~ ANY) ~> withContext(lineStatementNode) ~ StatementDelimiter
  }

  def ModuleName = Identifier

}
