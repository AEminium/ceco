package examples.pipeline

import scala.actors.Actor
import scala.actors.Actor._
import ceco._

case class Ack

class PreprocessingFault(val i:Int) extends ConcurrentException
class ProcessingFault(val i:Int) extends ConcurrentException


object Pipeline {
  def main(args : Array[String]) {
		ExceptionController.start
		Receiver.start
		Preprocessing.start
		Processing.start
		Dispatcher.start
		Receiver ! 1
  }
}

object Dispatcher extends PipelineUnit(ExceptionController) {
	var counter:List[Int] = List()
	def work(u:Int) = {
		println("[4] Dispatching " + u)
		Thread.sleep(1*1000)
		counter = u :: counter
		println("[4] Dispatched  " + counter)
		if (u == Receiver.expected) {
			Processing ! Stop
			Preprocessing ! Stop
			Receiver ! Stop
			ExceptionController ! Stop
			this ! Stop
		}
	}
}

object Processing extends PipelineUnit(Dispatcher) {
	var failFirstTime = true
	def work(u:Int) = {
		_async_try {
			println("[3] Processing " + u)
			if ( u == 3 && failFirstTime ) {
				println("[3] Fault at Processing. Stoping elements behind.")
				failFirstTime = false
				_throw(new ProcessingFault(u))
			}
			Thread.sleep(1*1000)
			println("[3] Processed " + u)
			next ! u
		} _catch {
			e:ProcessingFault => {
				discardAbove = e.i
				println("[3] Reason to stop: " + e.i)
			}
		}
	}
}

object Preprocessing extends PipelineUnit(Processing) {
	def work(u:Int) = {
		_async_try {
			println("[2] Preprocessing " + u)
			Thread.sleep(3*1000)
			println("[2] Preprocessed " + u)
			next ! u
		} _catch {
			e:ProcessingFault => {
				println("[2] Reason to stop: " + e.i)
				discardAbove = e.i
				_raise(new PreprocessingFault(e.i))
			}
		}
	}
}

object Receiver extends PipelineUnit(Preprocessing) {
	val expected = 5
	def work(u:Int) = {
		var done = false
		var start = u
		while (!done) {
			_async_try {
				println("[1] Receiving " + start)
				start.until(expected+1).foreach { 
					i => next ! i 
					Thread.sleep(3*1000)
				}
				done = true
			} _catch {
				e:PreprocessingFault => {
					println("[1] REBOOT TO: " + e.i)
					start = e.i
				}
			}
		}
	}
}

abstract class PipelineUnit(val next:Actor) extends Actor with ExceptionModel {
	var discardAbove = 1
	def work(u:Int):Unit
	def act {
		loop {
			react {
				case Stop => {
					exit()
				}
				case u:Int => {
					if ( u <= discardAbove) {
						work(u)
						discardAbove = discardAbove.max(u+1)
					}
				}
			}
		}
	}
}

