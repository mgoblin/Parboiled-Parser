package ru.mg.ast

object StatementTraceAstNode {

  def statementNode = { (nodeName: String) =>
    new StatementNode(nodeName, "", -1)
  }
}

sealed abstract class StatementTraceAstNode

case class StatementNode(
  val nodeName: String,
  val statement: String,
  val line: Long
) extends StatementTraceAstNode