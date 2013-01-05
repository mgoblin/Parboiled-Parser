package ru.mg.coverage

import ru.mg.parsing.esql.ast.EsqlAstNode
import ru.mg.parsing.broker.trace.ast.BrokerTraceAstNode

trait TraceFilter {

  protected def brokerTraces: List[BrokerTraceAstNode]

  protected def tracesForEsqlAstNode(node: EsqlAstNode) = {
    brokerTraces
  }
}
