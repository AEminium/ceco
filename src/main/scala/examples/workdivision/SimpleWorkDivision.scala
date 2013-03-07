package examples.workdivision

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._

object SimpleWorkDivision {
  def main(args: Array[String]) {
    PlainController.start
  }
}

case object Stop
case object AreThereErrors

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
    ErrorCounter.start
    val tree = TreeFactory.createRandomTree(0);
    val w = (new SimpleWorker).start

    val r: Any = w !? tree
    if ((ErrorCounter !? AreThereErrors).asInstanceOf[Boolean]) {
      println("Infinite value present in Tree")
    } else {
      println("Maximum: " + r.asInstanceOf[Int])
    }

    ErrorCounter ! Stop
    exit()
  }
}

class SimpleWorker extends Actor {
  def checkForErrors = {
    if ((ErrorCounter !? AreThereErrors).asInstanceOf[Boolean]) throw new InfiniteValue
  }
  def act() {
    loop {
      react {
        case e: Tree ⇒ {
          val ans = e match {
            case n: RealNode ⇒ n.value
            case InfNode ⇒ {
              ErrorCounter ! new InfiniteValue
              0
            }
            case t: Node ⇒ {
              try {
                checkForErrors
                val r = ((new SimpleWorker).start !! t.right)().asInstanceOf[Int]
                val l = ((new SimpleWorker).start !! t.left)().asInstanceOf[Int]
                checkForErrors
                if (r > l) r else l
              } catch {
                case e: InfiniteValue ⇒ {
                  println("Aborting Computation")
                  0
                }
              }
            }
          }
          sender ! ans
          exit()
        }
      }
    }
  }
}