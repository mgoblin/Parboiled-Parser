package ru.mg.parboiled

import org.parboiled.scala._


/**
 * Base class for expressions parser.
 * <p>
 *   Responsible for whitespace support and parsing expressions like<br/>
 * Primitive ~ Operation ~ Primitive with priority and parens<br/>
 * where <br/>
 * Primitive is a rule for number or string or boolean<br/>
 * and Operations is a list of operands. List string contains operators with same priority.
 * First items has higher priority.
 * </p>
 * <p>
 *   Descend classes need to override Primitive and Operations members.
 * </p>
 */
abstract class ExpressionParser extends Parser {

  def Primitive:Rule0
  def Operations:List[String]


  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule0 = rule { Operations.foldLeft(Factor)(expr) }

  def Factor = rule { Number | Parens }

  def Parens = rule { WhiteSpace ~ LParen ~ Expression ~ RParen ~ WhiteSpace}

  def Number = rule { WhiteSpace ~ Primitive ~ WhiteSpace}

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def LParen = "("
  def RParen = ")"

  def expr(e: Rule0, opGroup: String) = {e ~ zeroOrMore(anyOf(opGroup) ~ e)}
}