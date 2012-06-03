package ru.mg.esql.ast

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

}

sealed abstract class EsqlAstNode(val text: String, val linesRange: Range)

case class ModuleNode(
  override val text: String,
  override val linesRange: Range,
  statements: List[EsqlAstNode])
extends EsqlAstNode(text, linesRange: Range)

case class FunctionNode(
  override val text: String,
  override val linesRange: Range,
  statements: List[EsqlAstNode])
extends EsqlAstNode(text, linesRange: Range)

case class LineStatementNode(
  override val text: String,
  override val linesRange: Range)
extends EsqlAstNode(text, linesRange: Range)

abstract case class CommentNode(
  override val text: String,
  override val linesRange: Range)
extends EsqlAstNode(text, linesRange: Range)

case class LineCommentNode(
  override val text: String,
  override val linesRange: Range)
extends CommentNode(text, linesRange: Range)

case class BlockCommentNode (
  override val text: String,
  override val linesRange: Range
) extends CommentNode(text, linesRange: Range)

case class BeginEndNode (
  override val text: String,
  override val linesRange: Range
) extends LineStatementNode(text, linesRange: Range)

case class ExternalNode (
  override val text: String,
  override val linesRange: Range
) extends LineStatementNode(text, linesRange: Range)

case class SchemaNode (
  override val text: String,
  override val linesRange: Range
) extends LineStatementNode(text, linesRange: Range)

case class PathNode (
  override val text: String,
  override val linesRange: Range
) extends LineStatementNode(text, linesRange: Range)