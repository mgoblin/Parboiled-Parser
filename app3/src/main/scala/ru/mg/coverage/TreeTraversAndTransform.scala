package ru.mg.coverage

import scala.annotation.tailrec

trait TreeTraversAndTransform[A, B, Acc] {

  def getChildren(node: A): List[A]

  def transform(node: A): B

  def accumulate(outputNode: B, oldAccumulator: Acc): Acc

  val defaultAccumulator: Acc

  @tailrec
  final def traverseAndTransformTree( nodes: List[A], accumulator: Acc = defaultAccumulator): Acc = nodes match {

    case currentNode :: queueTail =>

      val children = getChildren(currentNode)
      val outputNode = transform(currentNode)
      val newAccumulator = accumulate(outputNode, accumulator)

      traverseAndTransformTree(children ::: queueTail, newAccumulator)

    case Nil =>
      accumulator
  }
}
