package pt.uc.dei.examples.async

import scala.actors.Actor
import scala.actors.Actor._
import pt.uc.dei.cehm._

class StopException extends ConcurrentException


object AsyncExample {
  def main(args : Array[String]) {
      Worker.start
      Controller.start
  }
}

object Controller extends Actor with ExceptionModel {   
  def act() {
		ExceptionController.start
		println("[Controller] Waiting for errors...")
		Thread.sleep(5*1000);
		println("[Controller] Got one error. Propagating...")
		_try {
			println("[Controller] Trying")
			_throw(new StopException)
		} _catch {
			e:Exception => () // We know what it is.
		}
		println("[Controller] Bye")
		ExceptionController ! Stop
	}
}


object Worker extends Actor with ExceptionModel {   
  def act() {
		_async_try {
    	println("[Worker] is working.")
			Thread.sleep(10*1000);
    	println("[Worker] is done.")
		} _catch {
			e:StopException => println("[Worker] Got an exception!")
		}
  }
}