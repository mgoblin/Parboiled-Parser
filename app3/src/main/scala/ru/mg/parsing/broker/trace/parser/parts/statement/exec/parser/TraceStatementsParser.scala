package ru.mg.parsing.broker.trace.statement.exec.parser

import org.parboiled.scala.parserunners.ReportingParseRunner
import ru.mg.parsing.broker.trace.filter.ESQLStatementTraceFilter
import ru.mg.parsing.broker.trace.parser.parts.common.CommonTraceParser


trait TraceStatementsParser {

  private val traceParser = new CommonTraceParser {
    override val buildParseTree = true
  }

  private val statementsFilter = new ESQLStatementTraceFilter()

  def parse(input: String) = {
    val run = ReportingParseRunner(traceParser.Trace).run(input)
    require(!run.hasErrors)

    statementsFilter.filterStatementExecutions(run.resultValue)
  }
}
