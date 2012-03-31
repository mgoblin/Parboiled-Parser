package ru.mg.esql.ast

import org.parboiled.Context

object AstNode {

  def moduleNode = {
    (header: String, statements: List[AstNode], footer: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new ModuleNode(header, lineNo, statements)
  }

  def functionNode = {
    (signature: String, body: List[AstNode], context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new FunctionNode(signature.trim(), lineNo, body)
  }

  def lineStatementNode = {
    (text: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new LineStatementNode(text.trim(), lineNo)
  }

  def commentNode = {
    (text: String, context: Context[_]) =>
      new LineCommentNode(text.trim(), context.getPosition.line)
  }

  def blockCommentNode = {
    (text: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new BlockCommentNode(text.trim(), lineNo)
  }

  def beginEndNode = {
     (body: String, context: Context[_]) =>
        val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
        new BeginEndNode(body.trim, lineNo)

  }

  def externalNode  = {
    (body: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      List(new ExternalNode(body.trim, lineNo))
  }

  def schemaNode  = {
    (text: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new SchemaNode(text, lineNo)
  }

  def pathNode  = {
    (text: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new PathNode(text, lineNo)
  }

}

sealed abstract class AstNode(val text: String, val startLine: Long)

case class ModuleNode(
  override val text: String,
  override val startLine: Long,
  statements: List[AstNode])
extends AstNode(text, startLine)

case class FunctionNode(
  override val text: String,
  override val startLine: Long,
  statements: List[AstNode])
extends AstNode(text, startLine)

case class LineStatementNode(
  override val text: String,
  override val startLine: Long)
extends AstNode(text, startLine)

abstract case class CommentNode(
  override val text: String,
  override val startLine: Long)
extends AstNode(text, startLine)

case class LineCommentNode(
  override val text: String,
  override val startLine: Long)
extends CommentNode(text, startLine)

case class BlockCommentNode (
  override val text: String,
  override val startLine: Long
) extends CommentNode(text, startLine)

case class BeginEndNode (
  override val text: String,
  override val startLine: Long
) extends AstNode(text, startLine)

case class ExternalNode (
  override val text: String,
  override val startLine: Long
) extends AstNode(text, startLine)

case class SchemaNode (
  override val text: String,
  override val startLine: Long
) extends AstNode(text, startLine)

case class PathNode (
  override val text: String,
  override val startLine: Long
) extends AstNode(text, startLine)