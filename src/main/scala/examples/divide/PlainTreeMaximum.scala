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

object ErrorCounter {
	var hasErrors = false
}

object PlainController extends Actor {

	def act() {
		val tree = TreeFactory.createRandomTree(0);
		val fjtask = new PlainFJMaximum
		fjtask.start

		val r:Any = fjtask !? tree
		if (ErrorCounter.hasErrors) {
			println("Infinite value present in Tree")
		} else {
			println("Maximum: " + r.asInstanceOf[Int])
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
							case Inf => {
								ErrorCounter.hasErrors = true
								new InfiniteValue
							}
						}
						case t:Node => {
							try {
								/* Recursive fork */
								def process(tr:Tree):Future[Any] = {
									val fjtask = new PlainFJMaximum
									fjtask.start
									fjtask !! tr
								}
								if (ErrorCounter.hasErrors) throw new InfiniteValue
								
								/* Merge the two results*/
								val r:Any = process(t.right)()
								val l:Any = process(t.left)()
								
								if (ErrorCounter.hasErrors) throw new InfiniteValue

								val ri = r.asInstanceOf[Int]
								val li = l.asInstanceOf[Int]
								if (ri > li) ri else li
							} catch {
								case e:InfiniteValue => {
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