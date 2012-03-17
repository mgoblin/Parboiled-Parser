package ru.mg.esql.ast

import org.parboiled.Context

object AstNode {

  def moduleNode = {
    (header: String, statements: List[AstNode], footer: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new ModuleNode(header.trim(), lineNo, statements)
  }

  def functionNode = {
    (header: String, signature: String, comment: Option[CommentNode], body: List[AstNode], context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new FunctionNode(signature.trim(), comment, lineNo, body)
  }

  def paramNode = {
    (header: String, context: Context[_]) =>
      val lineNo = context.getInputBuffer.getPosition(context.getStartIndex).line
      new ParamNode(header.trim(), lineNo)
  }

  def lineStatementNode = {
    (text: List[String], comment: Option[CommentNode], context: Context[_]) =>
      new LineStatementNode(text.mkString(" ").trim(), context.getPosition.line, comment)
  }

  def commentNode = {
    (text: String, context: Context[_]) =>
      new CommentNode(text.trim(), context.getPosition.line)
  }

}

sealed abstract class AstNode(val text: String, val startLine: Long)

abstract case class CompoundNode (
 override val text: String,
 override val startLine: Long,
 statements: List[AstNode])
extends AstNode(text, startLine)

abstract case class SimpleNode (
  override val text: String,
  override val startLine: Long)
extends AstNode(text, startLine)

case class ModuleNode(
  override val text: String,
  override val startLine: Long,
  override val statements: List[AstNode])
extends CompoundNode(text, startLine, statements)

case class FunctionNode(
  override val text: String,
  val comment: Option[CommentNode],
  override val startLine: Long,
  override val statements: List[AstNode])
extends CompoundNode(text, startLine, statements)

case class LineStatementNode(
  override val text: String,
  override val startLine: Long,
  comment: Option[CommentNode])
extends SimpleNode(text, startLine)

case class CommentNode(
  override val text: String,
  override val startLine: Long)
extends SimpleNode(text, startLine)

case class ParamNode(
  override val text: String,
  override val startLine: Long)
extends SimpleNode(text, startLine)
