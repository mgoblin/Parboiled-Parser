package ru.mg.parboiled.math

import ru.mg.parboiled.ExpressionParser


class CompareParser extends ExpressionParser {

  def Primitive = new MathParser() {override val buildParseTree = true}.Expression

  def Operations = List("<>")
}