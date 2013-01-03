package ru.mg.coverage

import scala.annotation.tailrec

trait TreeTraversal[A, B, Acc] {

  def getChildrenNodes(node: A): List[A]
  def transform(node: A): B
  def accumulate(outputNode: B, oldAccumulator: Acc): Acc
  val defaultAccumulator: Acc

  @tailrec
  final def traverseTree(
    nodes: List[A],
    accumulator: Acc = defaultAccumulator): Acc = {

    nodes match {

      case currentNode :: queueTail =>

        val children = getChildrenNodes(currentNode)
        val outputNode = transform(currentNode)
        val newAccumulator = accumulate(outputNode, accumulator)

        traverseTree(children ::: queueTail, newAccumulator)

      case Nil =>
        accumulator
    }
  }
}
