package ru.mg.trace

import org.parboiled.scala.parserunners.ReportingParseRunner


trait TraceStatementsParser {

  private val traceParser = new TraceParser {
    override val buildParseTree = true
  }

  private val statementsFilter = new StatementFilter()

  def parse(input: String) = {
    val run = ReportingParseRunner(traceParser.Trace).run(input)
    require(!run.hasErrors)

    statementsFilter.filterStatementExecutions(run.resultValue)
  }
}
