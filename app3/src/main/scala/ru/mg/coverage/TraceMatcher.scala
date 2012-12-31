package ru.mg.coverage


import ast.CoverageNode
import ru.mg.parsing.ast.{FunctionNode, ModuleNode, EsqlAstNode}
import ru.mg.parsing.broker.trace.ast.{BrokerTraceStatementNode, BrokerTraceAstNode}


class TraceMatcher(val traces: List[BrokerTraceAstNode]) extends TreeTraversal[EsqlAstNode, CoverageNode, List[CoverageNode]] {

  def getChildrenNodes[R >: EsqlAstNode](node: R) = getChildrenStatements(node)
  def transform[R >: EsqlAstNode](node: R) = makeCoverageNode(node)
  def accumulate[R >: CoverageNode](outputNode: R, oldAccumulator: List[CoverageNode]): List[CoverageNode] = {
    outputNode.asInstanceOf[CoverageNode] :: oldAccumulator
  }
  def defaultAccumulator(): List[CoverageNode] = List()

  private def getChildrenStatements[R >: EsqlAstNode](node: R) = {
    node match {
      case module: ModuleNode => module.statements
      case function: FunctionNode => function.statements
      case _ => Nil
    }
  }

  //TODO Учитывать в каком модуле расположена EsqlNode
  private def getTracesForNode[R >: EsqlAstNode](node: R): List[BrokerTraceAstNode] = {
    traces.filter(trace =>  {
      trace match {
        case statement: BrokerTraceStatementNode => node.asInstanceOf[EsqlAstNode].linesRange contains statement.esqlLineNo
        case _ => false
      }
    })
  }

  private def makeCoverageNode[R >: EsqlAstNode](esqlNode: R) = {
    new CoverageNode(esqlNode.asInstanceOf[EsqlAstNode], getTracesForNode(esqlNode))
  }

  def matchTraces(nodes: List[EsqlAstNode]) = { traverseTree(nodes) }

}
