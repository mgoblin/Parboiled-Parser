package ru.mg.parboiled.math

import org.parboiled.scala._


class MathParser extends Parser {

    def InputLine = rule { Expression ~ EOI }

    def Expression: Rule0 = rule { Term ~ zeroOrMore(anyOf("+-") ~ Term) }

    def Term = rule { Factor ~ zeroOrMore(anyOf("*/") ~ Factor) }

    def Factor = rule { Digits | Parens }

    def Parens = rule { "(" ~ Expression ~ ")" }

    def Digits = rule { oneOrMore(Digit) }

    def Digit = rule { "0" - "9" }
}