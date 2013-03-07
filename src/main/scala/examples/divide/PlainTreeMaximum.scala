package examples.divide

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random

case object Stop
case object AreThereErrors

object PlainTreeMaximum {
  def main(args: Array[String]) {
    PlainController.start
  }
}

object ErrorCounter extends Actor {
  var hasErrors = false
  def act() {
    loop {
      react {
        case e: InfiniteValue ⇒ hasErrors = true
        case AreThereErrors ⇒ sender ! hasErrors
        case Stop ⇒ exit()
      }
    }
  }
}

object PlainController extends Actor {

  def act() {
    val tree = TreeFactory.createRandomTree(0);
    ErrorCounter.start
    val fjtask = new PlainFJMaximum
    fjtask.start

    val r: Any = fjtask !? tree
    if ((ErrorCounter !? AreThereErrors).asInstanceOf[Boolean]) {
      println("Infinite value present in Tree")
    } else {
      println("Maximum: " + r.asInstanceOf[Int])
    }

    ErrorCounter ! Stop
    exit()
  }
}

class PlainFJMaximum extends Actor {
  def checkForErrors = {
    if ((ErrorCounter !? AreThereErrors).asInstanceOf[Boolean]) throw new InfiniteValue
  }
  def act() {
    loop {
      react {
        case e: Tree ⇒ {
          val ans = e match {
            /* Handle the base case*/
            case n: EmptyNode[_] ⇒ n.value match {
              case Real(n) ⇒ n // Just a number
              case Inf ⇒ {
                ErrorCounter ! new InfiniteValue
                0
              }
            }
            case t: Node ⇒ {
              try {
                /* Recursive fork */
                def process(tr: Tree): Future[Any] = {
                  val fjtask = new PlainFJMaximum
                  fjtask.start
                  fjtask !! tr
                }
                checkForErrors

                /* Merge the two results*/
                val r: Any = process(t.right)()
                val l: Any = process(t.left)()

                checkForErrors

                val ri = r.asInstanceOf[Int]
                val li = l.asInstanceOf[Int]
                if (ri > li) ri else li
              } catch {
                case e: InfiniteValue ⇒ {
                  println("Aborting Computation")
                  0
                }
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