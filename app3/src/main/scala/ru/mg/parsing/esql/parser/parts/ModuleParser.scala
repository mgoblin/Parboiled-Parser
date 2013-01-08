package ru.mg.parsing.esql.parser.parts

import ru.mg.parsing.esql.ast.{ModuleNode, EsqlAstNode}
import EsqlAstNode._
import org.parboiled.scala._


trait ModuleParser extends FunctionParser {

  def ModuleStatement: Rule1[ModuleNode] = rule {
    (ModuleDeclaration ~ ModuleBody ~ ModuleFooter) ~~> withContext(moduleNode)
  }

  def ModuleDeclaration: Rule1[String] = rule {
    def moduleDecl = { (x: String, y: String) =>
      y
    }
    CREATE ~ (ModuleType ~ MODULE ~ ModuleName) ~~> { moduleDecl } ~ WS
  }

  def ModuleType = rule {
    (COMPUTE | DATABASE | FILTER) ~> {_.toString }
  }

  def ModuleFooter = rule {
    END ~ MODULE ~> {
      _ => "END MODULE"
    } ~ StatementDelimiter
  }

  def ModuleBody = rule {
    zeroOrMore((Comment | FunctionStatement | DeclareStatement ) ~ WS)
  }

  def ModuleName = rule {
    oneOrMore("a" - "z" | "A" - "Z" | "0" - "9" | "_") ~> { _.toString }
  }

}
