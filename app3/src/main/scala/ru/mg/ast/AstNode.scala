package ru.mg.esql.ast

import org.parboiled.Context

object AstNode {

  def moduleNode = {
    (header: String, statements: List[AstNode], footer: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new ModuleNode(header, startLine, endLine, statements)
  }

  def functionNode = {
    (signature: String, body: List[AstNode], context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new FunctionNode(signature.trim(), startLine, endLine, body)
  }

  def lineStatementNode = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new LineStatementNode(text.trim(), startLine, endLine)
  }

  def commentNode = {
    (text: String, context: Context[_]) =>
      new LineCommentNode(text.trim(), context.getPosition.line, context.getPosition.line)
  }

  def blockCommentNode = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new BlockCommentNode(text.trim(), startLine, endLine)
  }

  def beginEndNode = {
     (body: String, context: Context[_]) =>
        val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
        val endLine = context.getPosition.line
        new BeginEndNode(body.trim, startLine, endLine)

  }

  def externalNode  = {
    (body: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      List(new ExternalNode(body.trim, startLine, endLine))
  }

  def schemaNode  = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new SchemaNode(text, startLine, endLine)
  }

  def pathNode  = {
    (text: String, context: Context[_]) =>
      val startLine = context.getInputBuffer.getPosition(context.getStartIndex).line
      val endLine = context.getPosition.line
      new PathNode(text, startLine, endLine)
  }

}

sealed abstract class AstNode(val text: String, val startLine: Long, val endLine: Long)

case class ModuleNode(
  override val text: String,
  override val startLine: Long,
  override val endLine: Long,
  statements: List[AstNode])
extends AstNode(text, startLine, endLine)

case class FunctionNode(
  override val text: String,
  override val startLine: Long,
  override val endLine: Long,
  statements: List[AstNode])
extends AstNode(text, startLine, endLine)

case class LineStatementNode(
  override val text: String,
  override val startLine: Long,
  override val endLine: Long)
extends AstNode(text, startLine, endLine)

abstract case class CommentNode(
  override val text: String,
  override val startLine: Long,
  override val endLine: Long)
extends AstNode(text, startLine, endLine)

case class LineCommentNode(
  override val text: String,
  override val startLine: Long,
  override val endLine: Long)
extends CommentNode(text, startLine, endLine)

case class BlockCommentNode (
  override val text: String,
  override val startLine: Long,
  override val endLine: Long
) extends CommentNode(text, startLine, endLine)

case class BeginEndNode (
  override val text: String,
  override val startLine: Long,
  override val endLine: Long
) extends LineStatementNode(text, startLine, endLine)

case class ExternalNode (
  override val text: String,
  override val startLine: Long,
  override val endLine: Long
) extends AstNode(text, startLine, endLine)

case class SchemaNode (
  override val text: String,
  override val startLine: Long,
  override val endLine: Long
) extends LineStatementNode(text, startLine, endLine)

case class PathNode (
  override val text: String,
  override val startLine: Long,
  override val endLine: Long
) extends LineStatementNode(text, startLine, endLine)