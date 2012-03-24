package ru.mg.esql.statements


import org.parboiled.scala._

trait ReservedWordsParser extends Parser {

  def TypeDecl = rule {
    T_BOOL | T_BOOLEAN | T_DATE | T_TIME | T_GMTTIME | T_TIMESTAMP | T_GMTTIMESTAMP | T_CHAR | T_CHARACTER |
    T_DEC | T_DECIMAL | T_FLOAT | T_INT | T_INTEGER  | T_ROW | T_BLOB | T_BIT | REFERENCE | T_REF | REFERENCE_TO
  }

  def T_BOOL = ignoreCase("BOOL")
  def T_BOOLEAN = ignoreCase("BOOLEAN")
  def T_DATE = ignoreCase("DATE")
  def T_TIME = ignoreCase("TIME")
  def T_GMTTIME = ignoreCase("GMTTIME")
  def T_TIMESTAMP = ignoreCase("TIMESTAMP")
  def T_GMTTIMESTAMP = ignoreCase("GMTTIMESTAMP")
  def T_CHAR = ignoreCase("CHAR")
  def T_CHARACTER = ignoreCase("CHARACTER")
  def T_DEC = ignoreCase("DEC")
  def T_DECIMAL = ignoreCase("DECIMAL")
  def T_FLOAT = ignoreCase("FLOAT")
  def T_INT = ignoreCase("INT")
  def T_INTEGER = ignoreCase("INTEGER")
  def T_ROW = ignoreCase("ROW")
  def T_BLOB = ignoreCase("BLOB")
  def T_BIT = ignoreCase("BIT")
  def REFERENCE = ignoreCase("REFERENCE")
  def T_REF = ignoreCase("REF")
  def REFERENCE_TO = ignoreCase("REFERENCE TO")


  def Identifier = rule {
    oneOrMore("a" - "z" | "A" - "Z" | "0" - "9" | "_" )
  }

  // Whitespace rule
  protected def WS: Rule0 = rule {
    zeroOrMore(anyOf(" \t\n\r\f"))
  }

  def RETURNS = ignoreCase("RETURNS")
  def LANGUAGE = ignoreCase("LANGUAGE")
  def ESQL = ignoreCase("ESQL")
  def DATABASE = ignoreCase("DATABASE")
  def JAVA = ignoreCase("JAVA")

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" ") || string.endsWith("\t") || string.endsWith("\n") || string.endsWith("\r") || string.endsWith("\f") )
      str(string.trim) ~ WS
    else
      str(string)

}
