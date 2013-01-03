package ru.mg.coverage


import ast.CoverageNode
import ru.mg.parsing.ast.{FunctionNode, ModuleNode, EsqlAstNode}
import ru.mg.parsing.broker.trace.ast.{BrokerTraceStatementNode, BrokerTraceAstNode}


class TraceMatcher(val traces: List[BrokerTraceAstNode]) extends TreeTraversal[EsqlAstNode, CoverageNode, List[CoverageNode]] {

  def getChildrenNodes(node: EsqlAstNode) = getChildrenStatements(node)
  def transform(node: EsqlAstNode) = makeCoverageNode(node)
  def accumulate(outputNode: CoverageNode, oldAccumulator: List[CoverageNode]): List[CoverageNode] = {
    outputNode :: oldAccumulator
  }
  val defaultAccumulator: List[CoverageNode] = List()

  private def getChildrenStatements(node: EsqlAstNode) = {
    node match {
      case module: ModuleNode => module.statements
      case function: FunctionNode => function.statements
      case _ => Nil
    }
  }

  //TODO Учитывать в каком модуле расположена EsqlNode
  private def getTracesForNode(node: EsqlAstNode): List[BrokerTraceAstNode] = {
    traces.filter(trace =>  {
      trace match {
        case statement: BrokerTraceStatementNode => node.linesRange contains statement.esqlLineNo
        case _ => false
      }
    })
  }

  private def makeCoverageNode(esqlNode: EsqlAstNode) = {
    new CoverageNode(esqlNode, getTracesForNode(esqlNode))
  }

  def matchTraces(nodes: List[EsqlAstNode]) = traverseTree(nodes)

}
