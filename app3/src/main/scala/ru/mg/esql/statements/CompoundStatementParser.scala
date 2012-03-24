package ru.mg.esql.statements

import org.parboiled.scala.rules.Rule1
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.CompoundStatementNode


trait CompoundStatementParser extends StatementParser {

  def BeginEnd: Rule1[CompoundStatementNode] = rule {
    Begin ~
      WS ~ zeroOrMore(Comment | BeginEnd | LineStatement)  ~
    End ~
    WS ~ StatementDelimiter ~
    WS ~ optional(LineComment) ~~> withContext { compoundStatementNode }
  }

  def Begin = rule {
    WS ~ ignoreCase("BEGIN") ~ WS ~ optional(LineComment)
  }

  def End = rule {
    WS ~ ignoreCase("END")
  }
}
