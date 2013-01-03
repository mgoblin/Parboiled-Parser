package ru.mg.parsing.esql.ast

import org.parboiled.Context

object EsqlAstNode {

  def moduleNode = {
    (header: String, statements: List[EsqlAstNode], footer: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new ModuleNode(header, startLine to endLine, statements)
  }

  def functionNode = {
    (signature: String, body: List[EsqlAstNode], context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new FunctionNode(signature.trim(), startLine to endLine, body)
  }

  def lineStatementNode = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new LineStatementNode(text.trim(), startLine to endLine)
  }

  def commentNode = {
    (text: String, context: Context[_]) =>
      new LineCommentNode(text.trim(), context.getPosition.line to context.getPosition.line)
  }

  def blockCommentNode = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new BlockCommentNode(text.trim(), startLine to endLine)
  }

  def beginEndNode = {
     (body: String, context: Context[_]) =>
        val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
        val endLine = context.getPosition.line
        new BeginEndNode(body.trim, startLine to endLine)

  }

  def externalNode  = {
    (body: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      List(new ExternalNode(body.trim, startLine to endLine))
  }

  def schemaNode  = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new SchemaNode(text, startLine to endLine)
  }

  def pathNode  = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new PathNode(text, startLine to endLine)
  }

  def setParent(node: EsqlAstNode, parent: EsqlAstNode) = node match {
      case m:   ModuleNode => m.copy(parent = Some(parent))
      case f:   FunctionNode => f.copy(parent = Some(parent))
      case be:  BeginEndNode => be.copy(parent = Some(parent))
      case ex:  ExternalNode => ex.copy(parent = Some(parent))
      case sc:  SchemaNode => sc.copy(parent = Some(parent))
      case pa:  PathNode => pa.copy(parent = Some(parent))
      case ls:  LineStatementNode => ls.copy(parent = Some(parent))
      case lc:  LineCommentNode => lc.copy(parent = Some(parent))
      case bc:  BlockCommentNode => bc.copy(parent = Some(parent))
  }

}

import EsqlAstNode._

sealed abstract class EsqlAstNode(val text: String, val linesRange: Range, val parent: Option[EsqlAstNode] = None)

case class ModuleNode(
  override val text: String,
  override val linesRange: Range,
  st: List[EsqlAstNode],
  override val parent: Option[EsqlAstNode] = None)
extends EsqlAstNode(text, linesRange: Range, parent)  {
  val statements = st.map { s: EsqlAstNode =>
    if (s.isInstanceOf[FunctionNode]) {
      val fn = s.asInstanceOf[FunctionNode]
      val functionStatements = fn.statements.map{ f: EsqlAstNode => setParent(f, fn) }
      setParent(fn.copy(statements = functionStatements), this)
    } else {
      setParent(s, this)
    }
  }
}

case class FunctionNode(
  override val text: String,
  override val linesRange: Range,
  statements: List[EsqlAstNode],
  override val parent: Option[EsqlAstNode] = None)
extends EsqlAstNode(text, linesRange: Range, parent)

case class LineStatementNode(
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None)
extends EsqlAstNode(text, linesRange: Range, parent)

abstract case class CommentNode(
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None)
extends EsqlAstNode(text, linesRange: Range, parent)

case class LineCommentNode(
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None)
extends CommentNode(text, linesRange: Range, parent)

case class BlockCommentNode (
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None
) extends CommentNode(text, linesRange: Range, parent)

case class BeginEndNode (
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None
) extends LineStatementNode(text, linesRange: Range, parent)

case class ExternalNode (
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None
) extends LineStatementNode(text, linesRange: Range, parent)

case class SchemaNode (
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None
) extends LineStatementNode(text, linesRange: Range, parent)

case class PathNode (
  override val text: String,
  override val linesRange: Range,
  override val parent: Option[EsqlAstNode] = None
) extends LineStatementNode(text, linesRange: Range, parent)