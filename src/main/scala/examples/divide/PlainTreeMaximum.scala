package examples.divide

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random

object PlainTreeMaximum {
  def main(args : Array[String]) {
      PlainController.start
  }
}


object PlainController extends Actor {
  
  def act() {
    val tree = TreeFactory.createRandomTree(0);
    val fjtask = new PlainFJMaximum
    fjtask.start
    
    val r:Any = fjtask !? tree
    
    r match {
      case i:Int => println("Maximum: " + i)
      case _ => println("Infinite value present in Tree")
    }
    exit()
  }
}

class PlainFJMaximum extends Actor {   
  def act() {
    loop {
      react {
        case e:Tree => {
          val ans = e match {
             /* Handle the base case*/
              case n:EmptyNode[_] => n.value match {
                  case Real(n) => n // Just a number
                  case Inf => new InfiniteValue
              }
              case t:Node => {
                /* Recursive fork */
                def process(tr:Tree):Future[Any] = {
                  val fjtask = new PlainFJMaximum
                  fjtask.start
                  fjtask !! tr
                }
                
                /* Merge the two results*/
                val r:Any = process(t.right)()
                val l:Any = process(t.left)()
                r match {
                  case e:Int => {
                    l match {
                      case e:Int => {
                        val ri = r.asInstanceOf[Int]
                        val li = l.asInstanceOf[Int]
                        if (ri > li) ri else li
                      } 
                      case _ => l
                    }
                  }
                  case _ => r
                }
                
              }
          }          
          /* Required to unblock parent call. */
          sender ! ans
          exit()
        }
      }
    }
  }
}