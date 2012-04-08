package ru.mg.trace

import org.parboiled.scala._


class TraceParser extends TimestampParser {

//
//  def Trace = rule { zeroOrMore(TraceLine) ~ EOI }
//
//  def TraceLine = rule {
//    Timestamp ~ ThreadId ~ LineType ~ RecordType ~ Message ~ NewLine
//  }
//
//
//  def ThreadId = rule {
//
//  }
//
//  def LineType = rule {
//
//  }
//
//  def RecordType = rule {
//
//  }
//
//  def Message = rule {
//
//  }
//
//  def NewLine = optional("\r") ~ "\n"
//
//  // Whitespace rule
//  def WS: Rule0 = rule {
//    zeroOrMore(anyOf(" \t\n\r\f"))
//  }
//  /**
//   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
//   * a blank, this keeps the rules free from most whitespace matching clutter
//   */
//  override implicit def toRule(string: String) =
//    if (string.endsWith(" "))
//      str(string.trim) ~ WS
//    else
//      str(string)
}
