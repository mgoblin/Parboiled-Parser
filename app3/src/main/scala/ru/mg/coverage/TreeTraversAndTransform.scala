package ru.mg.coverage

import scala.annotation.tailrec

trait TreeTraversAndTransform[A, B] {

  def getChildren(node: A): List[A]

  def transform(node: A, parent: Option[B]): B

  def accumulate(outputNode: B, oldAccumulator: List[B]): List[B]

  @tailrec
  final def traverseAndTransformTree( nodes: List[A], accumulator: List[B] = List(), parent: Option[B] = None): List[B] = nodes match {

    case currentNode :: queueTail =>

      val children = getChildren(currentNode)
      val outputNode = transform(currentNode, parent)
      val newAccumulator = accumulate(outputNode, accumulator)

      traverseAndTransformTree(children ::: queueTail, newAccumulator, Some(outputNode))

    case Nil =>
      accumulator
  }
}
