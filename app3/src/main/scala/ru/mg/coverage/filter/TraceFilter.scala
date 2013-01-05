package ru.mg.coverage.filter

import ru.mg.parsing.esql.ast.EsqlAstNode
import ru.mg.parsing.broker.trace.ast.{BrokerTraceStatementNode, BrokerTraceAstNode}

trait TraceFilter {

  protected def brokerTraces: List[BrokerTraceAstNode]

  protected def tracesForEsqlAstNode(node: EsqlAstNode) = {
    brokerTraces.filter { line: BrokerTraceAstNode =>
      line match {
        case trace: BrokerTraceStatementNode =>
          node.codePart == trace.codePart && node.linesRange.contains(trace.esqlLineNo)
        case _ => false
      }
    }
  }
}
