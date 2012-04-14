package ru.mg.ast

object StatementTraceAstNode {

  def statementNode = { (nodeName: String, statement: String, codePart: String) =>
    new StatementNode(nodeName, statement, codePart, -1)
  }
}

sealed abstract class StatementTraceAstNode

case class StatementNode(
  nodeName: String,
  statement: String,
  codePart: String,
  esqlLineNo: Long
) extends StatementTraceAstNode