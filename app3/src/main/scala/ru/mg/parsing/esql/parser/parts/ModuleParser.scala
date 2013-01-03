package ru.mg.parsing.esql.parser.parts

import ru.mg.parsing.esql.ast.EsqlAstNode
import EsqlAstNode._
import org.parboiled.scala._


trait ModuleParser extends FunctionParser {

  def ModuleStatement = rule {
    (ModuleHeader ~ ModuleBody ~ ModuleFooter) ~~> withContext(moduleNode)
  }

  def ModuleHeader = rule {
    def join = {
      x: String => x.split(" ").map(_.trim()).filter(!_.isEmpty).mkString(" ")
    }
    CREATE ~ (COMPUTE | DATABASE | FILTER) ~ MODULE ~ ModuleName ~ WS ~> join
  }

  def ModuleFooter = rule {
    END ~ MODULE ~> {
      _ => "END MODULE"
    } ~ StatementDelimiter
  }

  def ModuleBody = rule {
    zeroOrMore((Comment | FunctionStatement | DeclareStatement ) ~ WS)
  }

  def ModuleName = Identifier

}
