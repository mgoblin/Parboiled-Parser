package ru.mg.ast

object StatementTraceAstNode {

  def statementNode = { (nodeName: String, statement: String, codePart: String, lineNo: Long) =>
    new StatementNode(nodeName, statement, codePart, lineNo)
  }
}

sealed abstract class StatementTraceAstNode

case class StatementNode(
  nodeName: String,
  statement: String,
  codePart: String,
  esqlLineNo: Long
) extends StatementTraceAstNode