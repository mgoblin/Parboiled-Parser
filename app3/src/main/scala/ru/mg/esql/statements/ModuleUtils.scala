package ru.mg.esql.statements

import org.parboiled.scala.parserunners.ReportingParseRunner

object ModuleUtils {

  val moduleParser = new ModuleParser { override val buildParseTree = true }
  val functionParser = new FunctionParser { override val buildParseTree = true }
  val compoundStatementParser = new StatementParser { override val buildParseTree = true }

  def normalize = { x: String => x.split(" ").map(_.trim()).filter(!_.isEmpty).mkString(" ") }
  def normalizeList = { x: List[String] => x.map(_.trim()).filter(!_.isEmpty).mkString(" ") }

  def isModule = {
    x: String =>
      val isHeader = !ReportingParseRunner(moduleParser.ModuleHeader).run(x).hasErrors
      val isFooter = !ReportingParseRunner(moduleParser.ModuleFooter).run(x).hasErrors
      isHeader || isFooter
  }

  def isFunction = {
    x: String =>
      !ReportingParseRunner(functionParser.FunctionStatement).run(x).hasErrors
  }

//  def isBeginEnd = {
//    x: String => {
//        !ReportingParseRunner(compoundStatementParser.BeginEndStatementMask).run(x).hasErrors
//    }
//  }

  def isLineStatement = {
    x: String => !(isModule(x) || isFunction(x) /*|| isBeginEnd(x)*/)
  }

}
