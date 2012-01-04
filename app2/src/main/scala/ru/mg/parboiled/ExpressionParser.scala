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
 */abstract class ExpressionParser[R] extends Parser {

  def Primitive:Rule0
  def Operations:List[String]
  def calc(operation:String, op1:R, op2:R): R


  def value(s: String): R

  def InputLine = rule { Expression ~ EOI }

  def Expression:Rule1[R] = rule { Operations.foldLeft(Factor)(opGroup) }

  def Factor = rule { Number | Parens }

  def Parens = rule { WhiteSpace ~ LParen ~ Expression ~ RParen ~ WhiteSpace }

  def Number: Rule1[R] = rule { WhiteSpace ~ (Primitive ~> value) ~ WhiteSpace  }

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def LParen = "("
  def RParen = ")"

  def opGroup(e:Rule1[R], operators: String) =  {
    expr(e, operators.toList.map(_.toString))
  }

  def expr(e:Rule1[R], groupOps: List[String]) = {
    e ~ ( zeroOrMore(
        groupOps.map((op) => op ~ e ~~> ((op1:R, op2) => calc(op, op1, op2))).reduceLeft (_|_)))
  }

}