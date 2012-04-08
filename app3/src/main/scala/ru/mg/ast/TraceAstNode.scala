package ru.mg.ast


object TraceAstNode {
  def traceLineNode = { (timestamp: String, threadId: String, traceType: String, message: String) =>
    new TraceLineNode(timestamp, threadId, traceType, message)
  }
}

sealed abstract class TraceAstNode

case class TraceLineNode(
  val timestamp: String,
  val threadId: String,
  val traceType: String,
  val message: String
) extends TraceAstNode
