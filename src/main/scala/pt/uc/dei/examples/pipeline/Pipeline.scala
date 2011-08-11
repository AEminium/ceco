package pt.uc.dei.examples.pipeline

import scala.actors.Actor
import scala.actors.Actor._
import pt.uc.dei.cehm._

class PreprocessingFault extends ConcurrentException
class ProcessingFault extends ConcurrentException


object Pipeline {
  def main(args : Array[String]) {
		ExceptionController.start
		Receiver.start
		Preprocessing.start
		Processing.start
		Dispatcher.start

		1.until(10).foreach(Receiver ! _)
		
		Receiver ! Stop
  }
}

object Dispatcher extends PipelineUnit[Int](ExceptionController) {
	def work(u:Int) = {
		println("[Dis] Dispatching " + u)
		Thread.sleep(1*1000)
		println("[Dis] Dispatched  " + u)
	}
}

object Processing extends PipelineUnit[Int](Dispatcher) {
	def work(u:Int) = {
		_async_try {
			println("[Pro] Processing " + u)
			if ( u == 4) {
				println("[Pro] Fault at Processing. Stoping elements behind.")
				_throw(new ProcessingFault)
			}
			Thread.sleep(1*1000)
			println("[Pro] Processed " + u)
			next ! u
		} _catch {
			e:ProcessingFault => {
				println("[Pro] Reason to stop: " + e)
				alive = false
			}
		}
	}
}

object Preprocessing extends PipelineUnit[Int](Processing) {
	def work(u:Int) = {
		_async_try {
			println("[Pre] Preprocessing " + u)
			Thread.sleep(3*1000)
			println("[Pre] Preprocessed " + u)
			next ! u
		} _catch {
			e:ProcessingFault => {
				println("[Pre] Reason to stop: " + e)
				_raise(new PreprocessingFault)
				alive = false
			}
		}
	}
}

object Receiver extends PipelineUnit[Int](Preprocessing) {
	def work(u:Int) = {
		_async_try {
			println("[Rec] Receiving " + u)
			Thread.sleep(1*1000)
			next ! u
		} _catch {
			e:PreprocessingFault => {
				println("[Rec] Reason to stop: " + e)
				ExceptionController ! Stop
				alive = false
			}
		}
	}
}

abstract class PipelineUnit[K](val next:Actor) extends Actor with ExceptionModel {
	var alive = true
	def work(u:K):Unit
	def act {
		loop {
			react {
				case Stop => {
					next ! Stop
					exit()
				}
				case u:K => {
					if (alive) work(u)
					else this ! u
				}
			}
		}
	}
}

