package ru.mg.coverage


import ru.mg.parsing.ast.EsqlAstNode
import ru.mg.parsing.broker.trace.ast.BrokerTraceStatementNode


object Coverage {

  private def linkNodeTraces(esqlNode: EsqlAstNode, statementTraceNodes: List[BrokerTraceStatementNode]) = {
    //esqlNod
  }

  def nodesWithTraces(esqlNodes: List[EsqlAstNode], statementTraceNodes: List[BrokerTraceStatementNode]) = {
    esqlNodes.map(esqlNode => {
      val linkedTraceNodes = statementTraceNodes.filter(traceNode => esqlNode.linesRange contains traceNode.esqlLineNo)
      (esqlNode, linkedTraceNodes)
    })
  }
}
