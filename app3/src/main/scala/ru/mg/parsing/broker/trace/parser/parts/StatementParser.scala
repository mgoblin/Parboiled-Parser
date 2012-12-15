package ru.mg.parsing.broker.trace.parser.parts

import org.parboiled.scala._

trait StatementParser extends TimestampParser {

  def Node: Rule1[String] = rule {
    Prefix ~ "'" ~ NodeName ~> {
      _.toString
    } ~ "':"
  }

  def StatementTrace: Rule1[String] = rule {
    "Executing" ~ WS ~ "statement" ~ WS ~ "''" ~ Statement ~> {
      _.toString
    } ~ "''"
  }

  def CodePlace = rule {
    "at" ~ WS ~ "(" ~ CodePart ~ "," ~ WS ~ LineNo ~ ")."
  }

  def CodePart = rule {
    "'" ~ CodePartName ~> {
      _.toString
    } ~ "'"
  }

  def LineNo = rule {
    "'" ~ Digits ~> {
      _.toLong
    } ~ "." ~ Digits ~ "'"
  }

  def CodePartName = rule {
    rule {
      oneOrMore(!"'" ~ ANY)
    }
  }

  def NodeName = rule {
    oneOrMore(!"'" ~ ANY)
  }

  def Statement = rule {
    oneOrMore(!"''" ~ ANY)
  }

  def Digits = rule {
    oneOrMore(Digit)
  }

  def Digit = rule {
    "0" - "9"
  }

  def Prefix = rule {
    oneOrMore("A"-"Z" | Digit) ~ ": Node "
  }

  def WS: Rule0 = rule {
    oneOrMore(anyOf(" \t\n\r\f"))
  }
}
