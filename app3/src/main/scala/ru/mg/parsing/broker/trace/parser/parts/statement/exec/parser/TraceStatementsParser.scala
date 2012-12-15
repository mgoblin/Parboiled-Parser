package ru.mg.parsing.broker.trace.statement.exec.parser

import org.parboiled.scala.parserunners.ReportingParseRunner
import ru.mg.parsing.broker.trace.parser.TraceParser
import ru.mg.parsing.broker.trace.filter.ESQLStatementTraceFilter


trait TraceStatementsParser {

  private val traceParser = new TraceParser {
    override val buildParseTree = true
  }

  private val statementsFilter = new ESQLStatementTraceFilter()

  def parse(input: String) = {
    val run = ReportingParseRunner(traceParser.Trace).run(input)
    require(!run.hasErrors)

    statementsFilter.filterStatementExecutions(run.resultValue)
  }
}
