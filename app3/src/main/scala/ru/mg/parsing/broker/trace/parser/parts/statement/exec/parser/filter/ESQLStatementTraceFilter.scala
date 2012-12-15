package ru.mg.parsing.broker.trace.filter

import org.parboiled.scala.parserunners.ReportingParseRunner
import org.parboiled.scala.ParsingResult
import ru.mg.parsing.broker.trace.ast.BrokerTraceLineNode
import ru.mg.parsing.broker.trace.statement.exec.ast.StatementNode
import ru.mg.parsing.broker.trace.parser.parts.statement.exec.StatementTraceLineParser
import ru.mg.parsing.broker.trace.statement.exec.ast.StatementTraceAstNode._


class ESQLStatementTraceFilter {

  private val parser = new StatementTraceLineParser { override val buildParseTree = true }

  def filterStatementExecutions(traceLines: List[BrokerTraceLineNode]) = {
    traceLines.map(parseLineMessage).filter(isStatementExec).map(enrich)
  }

  private def parseLineMessage(traceLine: BrokerTraceLineNode) = {
    (traceLine, ReportingParseRunner(parser.StatementLine).run(traceLine.message))
  }

  private def isStatementExec(traceTuple: (BrokerTraceLineNode, ParsingResult[StatementNode])) = { !traceTuple._2.hasErrors }

  private def enrich(traceTuple: (BrokerTraceLineNode, ParsingResult[StatementNode])) =  { traceStatementNode(traceTuple) }
}
