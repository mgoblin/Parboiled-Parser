package ru.mg.esql.statements


import org.parboiled.scala._

trait ReservedWordsParser extends Parser {

  def TypeDecl = rule {
    T_BOOLEAN | T_BOOL | T_DATE | T_TIME | T_GMTTIME | T_TIMESTAMP | T_GMTTIMESTAMP | T_CHARACTER | T_CHAR |
    T_DECIMAL |T_DEC | T_FLOAT | T_INTEGER  | T_INT | T_ROW | T_BLOB | T_BIT | REFERENCE_TO | REFERENCE | T_REF
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

  def RETURNS = ignoreCase("RETURNS") ~ WS
  def LANGUAGE = ignoreCase("LANGUAGE") ~ WS
  def ESQL = ignoreCase("ESQL") ~ WS
  def DATABASE = ignoreCase("DATABASE") ~ WS
  def JAVA = ignoreCase("JAVA") ~ WS
  def CREATE = ignoreCase("CREATE") ~ WS
  def PROCEDURE = ignoreCase("PROCEDURE") ~ WS
  def FUNCTION = ignoreCase("FUNCTION") ~ WS
  def MODULE = ignoreCase("MODULE") ~ WS
  def END = ignoreCase("END") ~ WS
  def BEGIN = ignoreCase("BEGIN")
  def EXTERNAL = ignoreCase ("EXTERNAL") ~ WS
  def NAME = ignoreCase ("NAME") ~ WS
  def COMPUTE = ignoreCase("COMPUTE") ~ WS
  def FILTER = ignoreCase("FILTER") ~ WS
  def DECLARE = ignoreCase("DECLARE") ~ WS
  def BROKER = ignoreCase("BROKER") ~ WS
  def SCHEMA = ignoreCase("SCHEMA") ~ WS
  def PATH = ignoreCase("PATH") ~ WS

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ WS
    else
      str(string)

}
