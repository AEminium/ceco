package pt.uc.dei.examples.divide

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import pt.uc.dei.cehm._

class InfiniteValue extends Exception

sealed abstract class Tree
case class Node(left: Tree, right: Tree) extends Tree
case class EmptyNode[A](value:A) extends Tree

sealed abstract class Number
case class Real(n:Int) extends Number
case object Inf extends Number

object TreeMaximum {
  val rand = new Random
  
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
  
  def main(args : Array[String]) {
      val tree = createRandomTree(0);
      val fjtask = new FJMaximum
      fjtask.start
      val i:Any = fjtask !? tree
      println("Maximum: " + i)
  }
}

class FJMaximum extends Actor with ExceptionModel {   
  def act() {
    loop {
      react {
        case e:Tree => {
          val ans:Int = e match {
              case n:EmptyNode[_] => n.value match {
                  case Real(n) => n 
                  case Inf => -10000
              }
              case t:Node => {
                def process(tr:Tree):Any = {
                  val fjtask = new FJMaximum
                  fjtask.start
                  fjtask !? tr
                }
                val r:Int = process(t.right).asInstanceOf[Int]
                val l:Int = process(t.left).asInstanceOf[Int]
                if (r > l) r else l
              }
          }
          sender ! ans
          exit()
        }
      }
    }
  }
}