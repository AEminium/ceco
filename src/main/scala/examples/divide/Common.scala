package examples.divide

import scala.util.Random

/* Recursive Tree data structure */
sealed abstract class Tree
case class Node(left: Tree, right: Tree) extends Tree
case class EmptyNode[A](value:A) extends Tree

/* Number class with support for Infinite value */
sealed abstract class Number
case class Real(n:Int) extends Number
case object Inf extends Number


object TreeFactory {
  val rand = new Random
  /*
  Generates a new unbalanced tree with up to 10 levels,
  filled with numbers from 1 to 8 and Infinite.
  */
  def createRandomTree(n:Int):Tree = {
    if (rand.nextInt(10) < n) {
      def map(i:Int):Number = {
        i match {
            case 9 => Inf
            case _ => Real(i)
        }
      }
      EmptyNode[Number](map(rand.nextInt(10)))
    } else {
      Node(createRandomTree(n+1), createRandomTree(n+1))
    }
  }
}