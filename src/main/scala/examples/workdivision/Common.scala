package examples.workdivision

import scala.util.Random

/* Recursive Tree data structure */
sealed abstract class Tree
case class Node(left: Tree, right: Tree) extends Tree
case class RealNode(value: Int) extends Tree
case object InfNode extends Tree

object TreeFactory {
  val rand = new Random
  /*
  Generates a new unbalanced tree with up to 10 levels,
  filled with numbers from 1 to 8 and Infinite.
  */
  def createRandomTree(n: Int): Tree = {
    if (rand.nextInt(10) < n) {
     val i = rand.nextInt(10)
     i match {
        case 9 => InfNode
        case _ => RealNode(i)
      }
    } else {
      Node(createRandomTree(n + 1), createRandomTree(n + 1))
    }
  }
}