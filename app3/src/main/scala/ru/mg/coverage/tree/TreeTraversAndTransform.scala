package ru.mg.coverage.tree

import scala.annotation.tailrec

trait TreeTraversAndTransform[A, B] {

  protected def getChildren(node: A): List[A]

  protected def transform(node: A): B

  protected def accumulate(outputNode: B, oldAccumulator: List[B]): List[B]

  @tailrec
  final def traverseAndTransformTree( nodes: List[A], accumulator: List[B] = List()): List[B] = nodes match {

    case currentNode :: queueTail =>

      val children = getChildren(currentNode)
      val outputNode = transform(currentNode)
      val newAccumulator = accumulate(outputNode, accumulator)

      traverseAndTransformTree(children ::: queueTail, newAccumulator)

    case Nil =>
      accumulator
  }
}
