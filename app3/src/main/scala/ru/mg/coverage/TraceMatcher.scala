package ru.mg.coverage


import ast.CoverageNode
import ru.mg.parsing.ast.{FunctionNode, ModuleNode, EsqlAstNode}
import annotation.tailrec
import ru.mg.parsing.broker.trace.ast.BrokerTraceAstNode


class TraceMatcher(val traces: List[BrokerTraceAstNode]) {

  private def getChildrenStatements(node: EsqlAstNode) = {
    node match {
      case module: ModuleNode => module.statements
      case function: FunctionNode => function.statements
      case _ => Nil
    }
  }

  private def getTracesForNode(node: EsqlAstNode): List[BrokerTraceAstNode] = {
    traces
  }

  private def makeCoverageNode(esqlNode: EsqlAstNode) = {
    new CoverageNode(esqlNode, getTracesForNode(esqlNode))
  }

  @tailrec
  private [coverage] final def traverseTree(nodes: List[EsqlAstNode],
                   accumulator: List[CoverageNode],
                   transform: EsqlAstNode => CoverageNode): List[CoverageNode] = {
    nodes match {

      case currentNode :: queueTail =>
        val nodeStatements = getChildrenStatements(currentNode)
        val coverage = transform(currentNode)
        traverseTree(nodeStatements ::: queueTail, coverage :: accumulator, transform)

      case Nil =>
        accumulator.reverse
    }
  }

  def matchTraces(nodes: List[EsqlAstNode]) = {
    traverseTree(nodes, List(), makeCoverageNode)
  }

}
