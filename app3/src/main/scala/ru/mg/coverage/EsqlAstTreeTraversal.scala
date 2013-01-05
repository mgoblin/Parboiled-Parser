package ru.mg.coverage


import ast.CoverageNode
import ru.mg.parsing.esql.ast.EsqlAstNode


trait EsqlAstTreeTraversal extends TreeTraversAndTransform[EsqlAstNode, CoverageNode] {

  protected def getChildren(node: EsqlAstNode) = node match {
    case s: {def statements: List[EsqlAstNode]} => s.statements
    case _ => Nil
  }

  protected def accumulate(
    outputNode: CoverageNode,
    oldAccumulator: List[CoverageNode]): List[CoverageNode] = outputNode :: oldAccumulator
}
