package ru.mg.esql.statements

import org.parboiled.scala.rules.Rule1
import ru.mg.esql.ast.AstNode._
import ru.mg.esql.ast.CompoundStatementNode


trait CompoundStatementParser extends StatementParser {

  def BeginEndStatement: Rule1[CompoundStatementNode] = rule {
    Begin ~
      Body ~
      //zeroOrMore(Comment | BeginEndStatement | LineStatement)  ~
    End ~ StatementDelimiter ~~> withContext { compoundStatementNode }
  }

  def Body = rule {
    zeroOrMore(noneOf("END;")) ~~> { _ => new }
  }

  def Begin = rule {
    WS ~ ignoreCase("BEGIN")
  }

  def End = rule {
    WS ~ ignoreCase("END")
  }

  def BeginEndStatementMask = rule {
    BeginEndStatement | Begin ~> { _.toString } | End ~> { _.toString }
  }
}
