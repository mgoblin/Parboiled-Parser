package ru.mg.coverage


import ast.CoverageNode
import ru.mg.parsing.esql.ast.EsqlAstNode


trait EsqlAstTreeTraversal[A <: CoverageNode] extends TreeTraversAndTransform[EsqlAstNode, A, List[A]] {

  def getChildren(node: EsqlAstNode) = node match {
    case s: { def statements: List[EsqlAstNode] } => s.statements
    case _ => Nil
  }

  def accumulate(outputNode: A, oldAccumulator: List[A]): List[A] = {
    outputNode.esqlNode.parent match {
      case None =>  outputNode :: oldAccumulator
      case Some(x) => oldAccumulator
    }

  }
  val defaultAccumulator: List[A] = List()

}
