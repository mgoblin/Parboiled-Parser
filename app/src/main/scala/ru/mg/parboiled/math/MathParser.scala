package ru.mg.parboiled.math

import org.parboiled.scala._

class MathParser extends Parser {

  def Expression = rule { Term ~ zeroOrMore(anyOf("+-") ~ Term) }

  def Term = rule { oneOrMore("0" - "9") }

  def WhiteSpace = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ WhiteSpace
    else
      str(string)
}