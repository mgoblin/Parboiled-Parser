package ru.mg.coverage.tree

import ru.mg.parsing.esql.ast.EsqlAstNode
import ru.mg.coverage.ast.CoverageNode


trait EsqlAstTreeTraversal extends TreeTraversAndTransform[EsqlAstNode, CoverageNode] {

  protected def getChildren(node: EsqlAstNode) = node match {
    case s: {def statements: List[EsqlAstNode]} => s.statements
    case _ => Nil
  }

  protected def accumulate(
    outputNode: CoverageNode,
    oldAccumulator: List[CoverageNode]): List[CoverageNode] = outputNode :: oldAccumulator
}
