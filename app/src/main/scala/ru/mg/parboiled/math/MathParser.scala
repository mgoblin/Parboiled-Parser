package ru.mg.parboiled.math

import org.parboiled.scala._
import ru.mg.parboiled.ExpressionParser

class MathParser extends ExpressionParser {
  def Primitive = rule { oneOrMore("0" - "9") }
  def Operations = List("^%", "*/", "+-")
}