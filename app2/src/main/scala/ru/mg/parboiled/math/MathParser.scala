package ru.mg.parboiled.math

import org.parboiled.scala._
import ru.mg.parboiled.ExpressionParser

class MathParser extends ExpressionParser[Int] {

  def Primitive = rule { oneOrMore("0" - "9") ~> { _.toInt } }
  def Operations = List("*/", "-+")

  def calc(operation: Char, op1: Int, op2: Int) = operation match {
    case '+' => op1 + op2
    case '-' => op1 - op2
    case '*' => op1 * op2
    case '/' => op1 / op2
  }
}