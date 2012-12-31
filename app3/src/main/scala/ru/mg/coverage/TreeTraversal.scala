package ru.mg.coverage

import scala.annotation.tailrec

trait TreeTraversal[+A, +B, Acc] {

  def getChildrenNodes[R >: A](node: R): List[R]
  def transform[R >: A](node: R): B
  def accumulate[R >: B](outputNode: R, oldAccumulator: Acc): Acc
  def defaultAccumulator(): Acc

  @tailrec
  final def traverseTree[R >: A](
    nodes: List[R],
    accumulator: Acc = defaultAccumulator()): Acc = {

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
