package ru.mg.coverage

import ast.CoverageNode
import ru.mg.parsing.broker.trace.ast.BrokerTraceAstNode
import ru.mg.parsing.esql.ast.EsqlAstNode


class Coverage(val brokerTraces: List[BrokerTraceAstNode]) extends TraceFilter with  EsqlAstTreeTraversal{

  protected def transform(node: EsqlAstNode): CoverageNode = {
    new CoverageNode(node, tracesForEsqlAstNode(node))
  }

  def coverageForEsqNodes(nodes: List[EsqlAstNode]) = traverseAndTransformTree(nodes)
}
