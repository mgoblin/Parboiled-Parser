package ru.mg.coverage.ast

import ru.mg.parsing.esql.ast.{BlockCommentNode, LineCommentNode, EsqlAstNode}
import ru.mg.parsing.broker.trace.ast.BrokerTraceAstNode

class CoverageNode (
  val esqlNode : EsqlAstNode,
  val traces: List[BrokerTraceAstNode] ) {

  def ignore() = esqlNode match {
    case x: LineCommentNode => true
    case x: BlockCommentNode => true
    case _ => false
  }
}