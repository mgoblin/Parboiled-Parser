package ru.mg.coverage


import ast.CoverageNode
import ru.mg.parsing.ast.{FunctionNode, ModuleNode, EsqlAstNode}
import annotation.tailrec


object TraceMatcher {

  private def getChildrenStatements(node: EsqlAstNode) = {
    node match {
      case module: ModuleNode => module.statements
      case function: FunctionNode => function.statements
      case _ => Nil
    }
  }

  @tailrec
  def traverseTree(nodes: List[EsqlAstNode],
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

}
