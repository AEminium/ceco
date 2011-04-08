package pt.uc.dei.examples.divide

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import pt.uc.dei.cehm._

class InfiniteValue extends ConcurrentException

sealed abstract class Tree
case class Node(left: Tree, right: Tree) extends Tree
case class EmptyNode[A](value:A) extends Tree

sealed abstract class Number
case class Real(n:Int) extends Number
case object Inf extends Number

object TreeMaximum {
  def main(args : Array[String]) {
      Controller.start
  }
}

object Controller extends Actor with ExceptionModel {
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
  
  def act() {
    ExceptionController.start
    
    val tree = createRandomTree(0);
    val fjtask = new FJMaximum
    fjtask.start
    _try {
      val i:Any = fjtask !? tree
      _check
      println("Maximum: " + i)
    } _catch {
      e:InfiniteValue => println("Infinite value present in Tree")
    }
    
    
    ExceptionController ! Stop
    exit()
  }
}

class FJMaximum extends Actor with ExceptionModel {   
  def act() {
    loop {
      react {
        case e:Tree => {
          _try {
            _check
            val ans = e match {
              case n:EmptyNode[_] => n.value match {
                  case Real(n) => n 
                  case Inf => { 
                    println("Found an infinit")
                    _throw(new InfiniteValue)
                    0
                  }
              }
              case t:Node => {
                def process(tr:Tree):Future[Any] = {
                  val fjtask = new FJMaximum
                  fjtask.start
                  fjtask !! tr
                }
                _check
                val r:Int = process(t.right)().asInstanceOf[Int]
                val l:Int = process(t.left)().asInstanceOf[Int]
                if (r > l) r else l
              }
            }
            _check
            sender ! ans
          } _catch {
              e:InfiniteValue => {
                println("Computation aborted")
                sender ! 0
              }
          }
          exit()
        }
      }
    }
  }
}