package ru.mg.parboiled

import org.parboiled.scala._


abstract class ExpressionParser extends Parser {

  def Primitive:Rule0

  def Operations:List[String]



  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule0 = rule { Operations.foldLeft(Factor)(expr) }

  def Factor = rule { Number | Parens }

  def Parens = rule { WhiteSpace ~ lparen ~ Expression ~ rparent ~ WhiteSpace}

  def Number = rule { WhiteSpace ~ Primitive ~ WhiteSpace}

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def lparen = "("
  def rparent = ")"

  val expr = (e:Rule0, op: String) => {
    e ~ zeroOrMore(anyOf(op) ~ e)
  }

}