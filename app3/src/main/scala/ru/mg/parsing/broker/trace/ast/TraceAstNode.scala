package ru.mg.parsing.broker.trace.ast


object TraceAstNode {
  def traceLineNode = { (timestamp: String, threadId: String, traceType: String, message: String) =>
    new TraceLineNode(timestamp, threadId, traceType, message)
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
