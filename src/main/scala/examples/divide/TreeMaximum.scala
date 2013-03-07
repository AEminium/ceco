/*
This example calculate the maximum value contained inside
a binary tree with values on leaves.

Since infinite values are allowed, unnecessary computation
may be prevented using Concurrent exceptions.

*/

package examples.divide

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import ceco._

/*
Exception that should be raised shall an Infinite Value be found.
In order to catch it from other actors,
it must inherit from ConcurrentException
*/
class InfiniteValue extends ConcurrentException

/* Main method will only start the Controller method */
object TreeMaximum {
  def main(args: Array[String]) {
    Controller.start
  }
}

/*
The Controller actor requests the calculation.
To use ConcurrentExceptions, the actor integrates the
ExceptionModel trait.
*/
object Controller extends Actor with ExceptionModel {

  def act() {

    /* ExceptionController delivers exceptions to interested actors */
    ExceptionController.start

    val tree = TreeFactory.createRandomTree(0);
    val fjtask = new FJMaximum
    fjtask.start

    /* Concurrent try */
    _try {
      /* Synchronously request the maximum value. */
      val i: Any = fjtask !? tree

      /* Check wether any exceptions were raised. */
      _check

      println("Maximum: " + i)
    } _catch {
      /* Try block will only receive InfiniteValue exceptions. */
      e: InfiniteValue ⇒ println("Infinite value present in Tree")
    }

    /* Exception dispatching no longer required. */
    ExceptionController ! Stop
    exit()
  }
}

/*
FJMaximum actor calculates the maximum value using
a divide-and-conquer strategy. 

It forks into two other actors of the same kind
and awaits the answer from both and returns the maximum of both.

Whenever an Infinite value is found, all calculation actors
should abort their computations as they are no longer required.
*/
class FJMaximum extends Actor with ExceptionModel {
  def act() {
    loop {
      react {
        case e: Tree ⇒ {
          // Concurrent Try
          _try {
            _check // Check for exceptions before
            val ans = e match {
              /* Handle the base case*/
              case n: EmptyNode[_] ⇒ n.value match {
                case Real(n) ⇒ n // Just a number
                case Inf ⇒ {
                  println("Found an infinite value.")
                  /* Stop execution and inform other actors */
                  _throw(new InfiniteValue)
                  0 // Required for type checking
                }
              }
              case t: Node ⇒ {
                /* Recursive fork */
                def process(tr: Tree): Future[Any] = {
                  val fjtask = new FJMaximum
                  fjtask.start
                  fjtask !! tr
                }
                /* Re-check for Exceptions  */
                _check

                /* Merge the two results*/
                val r: Int = process(t.right)().asInstanceOf[Int]
                val l: Int = process(t.left)().asInstanceOf[Int]
                if (r > l) r else l
              }
            }
            _check
            /* Required to unblock parent call. */
            sender ! ans
          } _catch {
            e: InfiniteValue ⇒
              {
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