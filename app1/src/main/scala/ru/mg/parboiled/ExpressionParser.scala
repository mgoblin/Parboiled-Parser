package ru.mg.parboiled

import org.parboiled.scala._


/**
 * Trait for expressions parser.
 * <p>
 *   Responsible for whitespace support and parsing expressions like<br/>
 * Primitive ~ Operation ~ Primitive with priority and parens<br/>
 * where <br/>
 * Primitive is a rule for number or string or boolean<br/>
 * and Operations is a list of operands. List string contains operators with same priority.
 * First items has higher priority.
 * </p>
 * <p>
 *   Descend classes should override Primitive and Operations members.
 * </p>
 */
trait ExpressionParser extends Parser {

  /**
   * Primitive is a element of parsing. For example operand in math expression.
   * Descendants should define primitive
   * @return primitive rule
   */
  def Primitive: Rule0

  /**
   * List of available operations on primitives. Descendants should define operations.
   * Item with zero index has highest priority and etc.
   * Each item can contain operations group. One operation is group corresponds to one char.
   * @return operations list
   */
  def Operations: List[String]

  def LParen = "("
  def RParen = ")"


  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule0 = rule { Operations.foldLeft(Item)(OperationExpression) }

  protected def OperationExpression(e: Rule0, opGroup: String) = { e ~ zeroOrMore(anyOf(opGroup) ~ e) }

  protected def Item = rule { PrimitiveExpression | ParensExpression }

  protected def ParensExpression = rule { WhiteSpace ~ LParen ~ Expression ~ RParen ~ WhiteSpace}

  protected def PrimitiveExpression = rule { WhiteSpace ~ Primitive ~ WhiteSpace}

  protected def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

}