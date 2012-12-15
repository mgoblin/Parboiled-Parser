package ru.mg.parsing.broker.trace.statement.exec.ast

import org.parboiled.scala.ParsingResult
import ru.mg.parsing.broker.trace.ast.{TraceLineNode, TraceStatementNode}

object StatementTraceAstNode {

  def statementNode = { (nodeName: String, statement: String, codePart: String, lineNo: Long) =>
    new StatementNode(nodeName, statement, codePart, lineNo)
  }

  def traceStatementNode(traceTuple: (TraceLineNode, ParsingResult[StatementNode])) = {
    new  TraceStatementNode (
      traceTuple._1.timestamp,
      traceTuple._1.threadId,
      traceTuple._1.traceType,
      traceTuple._2.resultValue.nodeName,
      traceTuple._2.resultValue.statement,
      traceTuple._2.resultValue.codePart,
      traceTuple._2.resultValue.esqlLineNo
    )
  }
}

sealed abstract class StatementTraceAstNode

case class StatementNode(
  nodeName: String,
  statement: String,
  codePart: String,
  esqlLineNo: Long
) extends StatementTraceAstNode