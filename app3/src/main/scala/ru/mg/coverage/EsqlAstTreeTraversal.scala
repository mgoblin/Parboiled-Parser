package ru.mg.coverage


import ast.CoverageNode
import ru.mg.parsing.esql.ast.EsqlAstNode


trait EsqlAstTreeTraversal extends TreeTraversAndTransform[EsqlAstNode, CoverageNode] {

  def getChildren(node: EsqlAstNode) = node match {
    case s: {def statements: List[EsqlAstNode]} => s.statements
    case _ => Nil
  }

  def accumulate(
    outputNode: CoverageNode,
    oldAccumulator: List[CoverageNode]): List[CoverageNode] = outputNode.parent match {
      case None => outputNode :: oldAccumulator
      case Some(x) => oldAccumulator
  }
}
