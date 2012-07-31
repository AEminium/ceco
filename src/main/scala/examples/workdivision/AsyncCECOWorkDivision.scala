package examples.workdivision

import scala.actors.Actor
import scala.actors.Actor._
import ceco._

object AsyncCECOWorkDivision {
  def main(args: Array[String]) {
    AsyncController.start
  }
}

object AsyncController extends Actor with ExceptionModel {
  def act() {
    ExceptionController.start

    val tree = TreeFactory.createRandomTree(0);
    val worker = (new AsyncWorker).start

    _try {
      val i: Any = worker !? tree
      println("Maximum: " + i)
    } _catch {
      e: InfiniteValue => println("Infinite value present in Tree")
    }
    ExceptionController ! Stop
    exit()
  }
}

class AsyncWorker extends Actor with ExceptionModel {
  def act() {
    loop {
      react {
        case e: Tree => {
          _async_try {
            val ans = e match {
              case n: RealNode => n.value
              case InfNode => {
                  println("Found an infinite value.")
                  _throw(new InfiniteValue)
                  0
              }
              case t: Node => {
                val r = ((new Worker).start !! t.right)().asInstanceOf[Int]
                val l = ((new Worker).start !! t.left)().asInstanceOf[Int]
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