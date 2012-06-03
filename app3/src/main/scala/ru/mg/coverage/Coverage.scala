package ru.mg.coverage


import ru.mg.esql.ast.EsqlAstNode
import ru.mg.ast.TraceStatementNode


object Coverage {

  private def linkNodeTraces(esqlNode: EsqlAstNode, statementTraceNodes: List[TraceStatementNode]) = {
    //esqlNod
  }

  def nodesWithTraces(esqlNodes: List[EsqlAstNode], statementTraceNodes: List[TraceStatementNode]) = {
    esqlNodes.map(esqlNode => {
      val linkedTraceNodes = statementTraceNodes.filter(traceNode => esqlNode.linesRange contains traceNode.esqlLineNo)
      (esqlNode, linkedTraceNodes)
    })
  }
}
