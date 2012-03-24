package ru.mg.parboiled.math

import org.parboiled.scala._


class CompareParser extends Parser {

  val mathParser = new MathParser() { override val buildParseTree = true }

  def Primitive =  mathParser.Primitive | mathParser.LParen | mathParser.RParen

  def Operations = List("<>")

  def InputLine = rule { Expression ~ EOI }

  def Expression = rule { PrimitiveExpression ~ oneOrMore(anyOf(Operations(0)) ~ PrimitiveExpression) }

  protected def PrimitiveExpression = rule { WhiteSpace ~ Primitive ~ WhiteSpace}

  protected def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
}