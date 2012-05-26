package ru.mg.ast


object LineAstNode {
  def lineNode(esqlNodeName: String, syntacticPath: String, relativeLineNum: Long) = {
    new LineNode(esqlNodeName, syntacticPath, relativeLineNum)
  }
}

sealed abstract class LineAstNode

case class LineNode (
  val esqlNodeName: String,
  val syntacticPath: String,
  val relativeLineNum: Long
)
