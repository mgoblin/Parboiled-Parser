package ru.mg.ast

import org.parboiled.scala.ParsingResult


object TraceAstNode {
  def traceLineNode = { (timestamp: String, threadId: String, traceType: String, message: String) =>
    new TraceLineNode(timestamp, threadId, traceType, message)
  }

  def traceStatementNode(traceTuple: (TraceLineNode, ParsingResult[StatementNode])) = {
    new  TraceStatementNode(
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

sealed abstract class TraceAstNode (
  timestamp: String,
  threadId: String,
  traceType: String
)

case class TraceLineNode(
  timestamp: String,
  threadId: String,
  traceType: String,
  message: String
) extends TraceAstNode(timestamp, threadId, traceType)

case class TraceStatementNode(
  timestamp: String,
  threadId: String,
  traceType: String,
  nodeName: String,
  statement: String,
  codePart: String,
  esqlLineNo: Long
) extends TraceAstNode(timestamp, threadId, traceType)
