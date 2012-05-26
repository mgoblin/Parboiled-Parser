package ru.mg.trace

import ru.mg.trace.esql.StatementTraceLineParser
import org.parboiled.scala.parserunners.ReportingParseRunner
import ru.mg.ast.{StatementNode, TraceLineNode}
import org.parboiled.scala.ParsingResult
import ru.mg.ast.TraceAstNode._


class StatementFilter {

  val parser = new StatementTraceLineParser { override val buildParseTree = true }

  def filterStatementExecutions(traceLines: List[TraceLineNode]) = {
    traceLines.map(parseLineMessage).filter(isStatementExec).map(enrich)
  }

  def parseLineMessage(traceLine: TraceLineNode) = {
    (traceLine, ReportingParseRunner(parser.StatementLine).run(traceLine.message))
  }

  def isStatementExec(traceTuple: (TraceLineNode, ParsingResult[StatementNode])) = { !traceTuple._2.hasErrors }

  def enrich(traceTuple: (TraceLineNode, ParsingResult[StatementNode])) =  { traceStatementNode(traceTuple) }
}
