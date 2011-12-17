package ceco

import scala.collection.mutable.{Queue, SynchronizedQueue,HashMap}
import scala.actors.Actor
import scala.actors.OutputChannel
import scala.actors.Actor._

trait ExceptionModel extends Actor {
  
  val exceptionQueue:Queue[Exception] = new SynchronizedQueue[Exception]()

  /* Overwrite TryCatch and TryCatchFinally Statements */
  def _tryF(code: => Unit) {
    new TryCatchFinally(this, wrapBlock(code))
  }

	def _async_tryF(code: => Unit) = {
    new AsyncTryCatchFinally(this, wrapBlock(code))
  }

  def _try(code: => Unit) = {
    new TryCatch(this, wrapBlock(code))
  }

	def _async_try(code: => Unit) = {
    new AsyncTryCatch(this, wrapBlock(code))
  }
  
  def _throw(e:Exception) {
    _raise(e)
    throw e
  }
  
  def _raise(e:Exception) {
    ExceptionController ! e
  }
  
  def _check = {
    if (!exceptionQueue.isEmpty) throw exceptionQueue.dequeue
  }
  
	/* Internal stuff */
	
  def _receive(e:Exception) {
    exceptionQueue enqueue e
  }

	private def wrapBlock(code: => Unit) = {
		val f:( () => Unit) = { () => code }
		f
	}
  
}